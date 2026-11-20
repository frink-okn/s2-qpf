package org.renci.frink.kgf

import java.security.MessageDigest

/** A serialization this server offers. `format=` names one; `Accept` negotiates among them (KGF doc 03 section 3.1 principle 3).
  *
  * The set per operation matches `kgf-rs`, so a client written against one KGF server reads this one. That set does not include CSV,
  * Parquet or Arrow, which doc 03 makes peer formats of every bindings-shaped operation and no KGF server offers yet; `docs/kgf.md` section
  * 4 records the divergence rather than leaving a client to discover it.
  */
enum Format(val name: String, val mediaType: String, val textual: Boolean = true):
  case Json extends Format("json", "application/json")
  case NQuads extends Format("nq", "application/n-quads")
  case TriG extends Format("trig", "application/trig")
  case Turtle extends Format("ttl", "text/turtle")
  case JsonLd extends Format("jsonld", "application/ld+json")
  case Markdown extends Format("md", "text/markdown")
  case Html extends Format("html", "text/html")

  def contentType: String = if textual then s"$mediaType; charset=utf-8" else mediaType

object Format:

  /** The data operations: JSON first, so it is what an unspecific `Accept` gets */
  val Data: Seq[Format] = Vector(Json, NQuads, TriG, Turtle, JsonLd, Html)

  /** Operations whose answer is a table or an envelope rather than quads */
  val Tabular: Seq[Format] = Vector(Json, Html)

  val Rdf: Seq[Format] = Vector(Turtle, JsonLd, Html)

  val Card: Seq[Format] = Vector(Markdown, Json, Html)

  def negotiate(offered: Seq[Format], requested: Option[String], accept: Option[String]): Either[Problem, Format] =
    requested.map(_.trim).filter(_.nonEmpty) match
      case Some(name) =>
        offered
          .find(_.name == name)
          .map(Right(_))
          .getOrElse(
            Left(
              Problem(
                Code.UnsupportedFormat,
                s"format=$name is not a serialization this operation offers; it offers ${offered.map(_.name).mkString(", ")}"
              )
            )
          )
      case None => fromAccept(offered, accept)

  private def fromAccept(offered: Seq[Format], accept: Option[String]): Either[Problem, Format] =
    accept.map(_.trim).filter(_.nonEmpty).map(parseAccept).filter(_.nonEmpty) match
      case None => Right(offered.head)
      case Some(ranges) =>
        val acceptable = for
          (range, _) <- ranges
          format <- offered
          if matches(range, format.mediaType)
        yield format
        acceptable.headOption
          .map(Right(_))
          .getOrElse(
            Left(
              Problem(
                Code.NotAcceptable,
                s"no acceptable representation; this operation offers ${offered.map(_.mediaType).mkString(", ")}. " +
                  "Relax Accept, or name one with format="
              )
            )
          )

  /** Media ranges with a positive quality, most wanted first. A range with `q=0` is a refusal, so it is dropped rather than ordered last.
    */
  private def parseAccept(value: String): Seq[(String, Double)] =
    value
      .split(',')
      .toVector
      .flatMap { entry =>
        val parts = entry.split(';').toVector.map(_.trim).filter(_.nonEmpty)
        parts.headOption.flatMap { range =>
          val quality = parts.tail
            .find(_.toLowerCase.startsWith("q="))
            .flatMap(parameter => parameter.drop(2).trim.toDoubleOption)
            .getOrElse(1.0)
          Option.when(quality > 0)(range.toLowerCase -> quality)
        }
      }
      .sortBy(-_._2)

  private def matches(range: String, mediaType: String): Boolean =
    range == "*/*" || range == mediaType || range == s"${mediaType.takeWhile(_ != '/')}/*"

/** What a response says about its own completeness, in the envelope and in the headers alike, so that every representation carries it (KGF
  * doc 03 section 3.6). Silent truncation is prohibited.
  */
final case class Completeness(complete: Boolean, truncation: Option[String] = None, next: Option[String] = None):
  def headers: Seq[(String, String)] =
    Vector("KGF-Complete" -> complete.toString) ++
      truncation.map("KGF-Truncation-Reason" -> _) ++
      next.map("KGF-Next-Cursor" -> _)

  def fields: Seq[(String, Json)] =
    Vector("complete" -> Json.Bool(complete)) ++
      truncation.map(reason => "truncation_reason" -> Json.Str(reason)) ++
      Vector("next" -> next.map(Json.Str(_)).getOrElse(Json.Null))

object Completeness:
  val Complete = Completeness(true)

  /** A page that ends at its limit with more to come */
  def pageLimit(cursor: String): Completeness = Completeness(false, Some("page_limit"), Some(cursor))

object Headers:

  /** A versioned resource is immutable: a release of a computed graph is the code it runs, so an answer at this URL never changes. */
  val Immutable = "public, max-age=31536000, immutable"

  val Redirect = "public, max-age=300"

  def apply(format: Format, request: String, completeness: Completeness, caching: String = Immutable): Seq[(String, String)] =
    Vector(
      "Cache-Control" -> caching,
      "ETag" -> etag(format, request),
      "Vary" -> "Accept"
    ) ++ completeness.headers

  /** Representation-specific, since one URL serves several formats, and bound to the release, since that is this dataset's content */
  private def etag(format: Format, request: String): String =
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(s"${Service.ContentDigest}\n$request".getBytes("UTF-8"))
    val hash = digest.digest().take(8).map(byte => f"${byte & 0xff}%02x").mkString
    s"""W/"${Service.ContentDigest}.$hash.${format.name}""""
