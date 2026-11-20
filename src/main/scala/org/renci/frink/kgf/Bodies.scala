package org.renci.frink.kgf

import org.apache.jena.atlas.json.JSON
import org.apache.jena.atlas.json.JsonObject
import org.apache.jena.atlas.json.JsonValue
import org.apache.jena.graph.Node
import org.renci.frink.kgf.Operations.PatternRequest

import scala.jdk.CollectionConverters.*
import scala.util.Try

/** The body of a bindings-restricted request, sent by QUERY or by POST with identical semantics (KGF doc 03 sections 3.1 and 3.4.2):
  *
  * {{{
  * {"pattern": {"s": "?cell", "p": "<…/sfTouches>", "o": "?n"},
  *  "bindings": {"vars": ["?cell"], "rows": [["<…/s2.level13.5522341869704445952>"]]},
  *  "g": "*", "limit": 1000, "cursor": "…"}
  * }}}
  *
  * A term is written in the parameter syntax of section 3.3, or as the term object a response row uses, which is the canonical form for
  * terms that are awkward to escape. `null` in a row is `UNDEF`, which leaves that variable unbound for that row. A field this server does
  * not know is refused rather than ignored, so a misspelling is reported instead of silently changing the answer.
  */
object Bodies:

  final case class Body(pattern: PatternRequest, rows: Seq[Map[String, Node]], limit: Option[Int], cursor: Option[String])

  private val patternFields = Set("s", "p", "o", "o.dt", "o.lang")

  def parse(text: String, fields: Set[String]): Either[Problem, Body] =
    for
      body <- Try(JSON.parse(text)).toOption.toRight(Problem.malformed("the request body is not a JSON object"))
      _ <- unknown(body, fields)
      pattern <- patternOf(body)
      bindings <- bindingsOf(body)
      limit <- optionalInt(body, "limit")
      cursor <- optionalString(body, "cursor")
    yield Body(pattern, bindings, limit, cursor)

  private def unknown(body: JsonObject, fields: Set[String]): Either[Problem, Unit] =
    body.keys().asScala.find(key => !fields.contains(key)) match
      case Some(key) =>
        Left(Problem.malformed(s"the request body has an unknown field `$key`; it takes ${fields.toVector.sorted.mkString(", ")}"))
      case None => Right(())

  private def patternOf(body: JsonObject): Either[Problem, PatternRequest] =
    val pattern = Option(body.get("pattern")).filter(_.isObject).map(_.getAsObject)
    for
      _ <- pattern.map(unknown(_, patternFields)).getOrElse(Right(()))
      graph <- Option(body.get("g")).map(value => spell(value).map(Some(_))).getOrElse(Right(None))
      parsed <- PatternRequest.parse(
        pattern.flatMap(position("s")),
        pattern.flatMap(position("p")),
        pattern.flatMap(position("o")),
        graph,
        pattern.flatMap(position("o.dt")),
        pattern.flatMap(position("o.lang"))
      )
    yield parsed

  private def position(name: String)(pattern: JsonObject): Option[String] =
    Option(pattern.get(name)).filterNot(_.isNull).flatMap(value => spell(value).toOption)

  private def bindingsOf(body: JsonObject): Either[Problem, Seq[Map[String, Node]]] =
    Option(body.get("bindings")).filterNot(_.isNull) match
      case None => Right(Seq.empty)
      case Some(value) if !value.isObject =>
        Left(Problem.malformed("`bindings` takes an object with `vars` and `rows`"))
      case Some(value) =>
        val bindings = value.getAsObject
        for
          _ <- unknown(bindings, Set("vars", "rows"))
          variables <- Option(bindings.get("vars"))
            .filter(_.isArray)
            .map(_.getAsArray.asScala.toVector)
            .toRight(Problem.malformed("`bindings.vars` is an array of variable names"))
          names <- variables
            .map(variable =>
              Option(variable)
                .filter(_.isString)
                .map(_.getAsString.value().stripPrefix("?"))
                .toRight(Problem.malformed("`bindings.vars` holds variable names, as strings"))
            )
            .foldLeft[Either[Problem, Vector[String]]](Right(Vector.empty))((found, name) => found.flatMap(all => name.map(all :+ _)))
          rawRows <- Option(bindings.get("rows"))
            .filter(_.isArray)
            .map(_.getAsArray.asScala.toVector)
            .toRight(Problem.malformed("`bindings.rows` is an array of rows"))
          _ <- Either.cond(
            rawRows.size <= Limits.MaxBindings,
            (),
            Problem.capExceeded(s"a bindings body holds at most ${Limits.MaxBindings} rows, and this one holds ${rawRows.size}")
          )
          rows <- rawRows.zipWithIndex
            .map((row, index) => rowOf(names, row, index))
            .foldLeft[Either[Problem, Vector[Map[String, Node]]]](Right(Vector.empty))((found, row) =>
              found.flatMap(all => row.map(all :+ _))
            )
        yield rows

  private def rowOf(names: Seq[String], row: JsonValue, index: Int): Either[Problem, Map[String, Node]] =
    if !row.isArray then Left(Problem.malformed(s"binding row $index is not an array of terms"))
    else
      val values = row.getAsArray.asScala.toVector
      if values.size != names.size then Left(Problem.malformed(s"binding row $index has ${values.size} terms for ${names.size} variables"))
      else
        values
          .zip(names)
          .foldLeft[Either[Problem, Map[String, Node]]](Right(Map.empty)) { (found, pair) =>
            val (value, name) = pair
            found.flatMap { bound =>
              // null is UNDEF: the variable stays unbound for this row, and restricts nothing
              if value == null || value.isNull then Right(bound)
              else spell(value).flatMap(Term.parse).map(node => bound + (name -> node))
            }
          }

  /** A term in the parameter syntax of section 3.3, from either spelling a body may use */
  private def spell(value: JsonValue): Either[Problem, String] =
    if value.isString then Right(value.getAsString.value())
    else if value.isObject then
      val term = value.getAsObject
      val lexical = Option(term.get("value")).filter(_.isString).map(_.getAsString.value())
      val kind = Option(term.get("type")).filter(_.isString).map(_.getAsString.value())
      (kind, lexical) match
        case (Some("iri"), Some(iri))     => Right(s"<$iri>")
        case (Some("uri"), Some(iri))     => Right(s"<$iri>")
        case (Some("bnode"), Some(label)) => Right(s"_:$label")
        case (Some("literal"), Some(form)) =>
          val language = Option(term.get("lang")).filter(_.isString).map(_.getAsString.value())
          val datatype = Option(term.get("datatype")).filter(_.isString).map(_.getAsString.value())
          Right(language.map(tag => s""""$form"@$tag""").orElse(datatype.map(iri => s""""$form"^^<$iri>""")).getOrElse(s""""$form""""))
        case (Some("variable"), Some(name)) => Right(s"?$name")
        case _ => Left(Problem.badTerm("a term object takes a `type` of iri, literal, bnode or variable, and a `value`"))
    else Left(Problem.badTerm(s"$value is not a term: write one as a string or as a term object"))

  private def optionalInt(body: JsonObject, name: String): Either[Problem, Option[Int]] =
    Option(body.get(name)).filterNot(_.isNull) match
      case None                          => Right(None)
      case Some(value) if value.isNumber => Right(Some(value.getAsNumber.value().intValue()))
      case Some(_)                       => Left(Problem.malformed(s"`$name` is a number"))

  private def optionalString(body: JsonObject, name: String): Either[Problem, Option[String]] =
    Option(body.get(name)).filterNot(_.isNull) match
      case None                          => Right(None)
      case Some(value) if value.isString => Right(Some(value.getAsString.value()))
      case Some(_)                       => Left(Problem.malformed(s"`$name` is a string"))
