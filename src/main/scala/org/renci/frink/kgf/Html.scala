package org.renci.frink.kgf

import scalatags.Text.all.*
import scalatags.Text.tags2.title as titleTag

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/** The HTML representation of every KGF resource.
  *
  * One renderer over the JSON a resource already produces, rather than a page per operation: the JSON is the answer, and this shows it with
  * its IRIs followed. An array of objects becomes a table, anything else a list of fields.
  */
object Html:

  private def encode(value: String): String = URLEncoder.encode(value, StandardCharsets.UTF_8)

  private val styles = """
    |body { font-family: system-ui, sans-serif; margin: 0 auto; max-width: 70rem; padding: 1.5rem; line-height: 1.5; color: #1a1a1a; }
    |header { border-bottom: 1px solid #ddd; margin-bottom: 1.5rem; padding-bottom: 0.75rem; }
    |header a { margin-right: 0.75rem; }
    |h1 { font-size: 1.4rem; margin: 0 0 0.5rem; }
    |table { border-collapse: collapse; width: 100%; margin: 0.5rem 0 1.5rem; }
    |th, td { border-bottom: 1px solid #eee; padding: 0.35rem 0.6rem; text-align: left; vertical-align: top; font-size: 0.9rem; }
    |th { background: #f7f7f7; font-weight: 600; }
    |td.number { text-align: right; font-variant-numeric: tabular-nums; }
    |dl { display: grid; grid-template-columns: max-content 1fr; gap: 0.2rem 1rem; margin: 0 0 1.25rem; }
    |dt { font-weight: 600; }
    |dd { margin: 0; }
    |code, pre { font-family: ui-monospace, monospace; font-size: 0.85rem; }
    |pre { background: #f7f7f7; overflow-x: auto; padding: 0.75rem; }
    |.tag { color: #666; font-size: 0.8rem; }
    |footer { border-top: 1px solid #ddd; color: #666; font-size: 0.85rem; margin-top: 2rem; padding-top: 0.75rem; }
    |""".stripMargin

  def page(heading: String, parts: Frag*): String =
    val operations = Service.Operations.map(operation => a(href := Service.link(Service.operationPath(operation)), operation))
    "<!DOCTYPE html>" + html(
      head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        titleTag(s"$heading — ${Service.Title}"),
        tag("style")(raw(styles))
      ),
      body(
        tag("header")(
          h1(a(href := Service.link(Service.DatasetPath), Service.Title), raw(" &middot; "), heading),
          div(a(href := Service.link("/"), "service"), operations)
        ),
        parts,
        tag("footer")(
          s"${Service.DatasetId} ${Service.Version} — a computed graph: every quad is computed on request. ",
          a(href := "https://github.com/frink-okn/s2-qpf", "s2-qpf")
        )
      )
    ).render

  /** A JSON answer as HTML */
  def render(value: Json): Frag = value match
    case Json.Obj(fields) => fieldList(fields)
    case Json.Arr(items)  => table(items)
    case other            => span(plain(other))

  private def fieldList(fields: Seq[(String, Json)]): Frag =
    frag(
      dl(fields.collect { case (name, value) if !isCollection(value) => frag(dt(name), dd(valueOf(value))) }),
      fields.collect {
        case (name, Json.Arr(items)) if items.nonEmpty          => frag(h2(name), table(items))
        case (name, nested: Json.Obj) if nested.fields.nonEmpty => frag(h2(name), fieldList(nested.fields))
      }
    )

  private def isCollection(value: Json): Boolean = value match
    case Json.Arr(items)   => items.nonEmpty
    case Json.Obj(entries) => entries.nonEmpty && term(value).isEmpty
    case _                 => false

  /** An array of objects as a table, anything else as a list */
  private def table(items: Seq[Json]): Frag =
    val rows = items.collect { case Json.Obj(fields) => fields }
    if rows.size == items.size && rows.nonEmpty then
      val columns = rows.flatMap(_.map(_._1)).distinct
      tag("table")(
        thead(tr(columns.map(th(_)))),
        tbody(
          rows.map(row =>
            tr(columns.map { column =>
              val cell = row.find(_._1 == column).map(_._2).getOrElse(Json.Null)
              td(cls := (if isNumber(cell) then "number" else ""))(valueOf(cell))
            })
          )
        )
      )
    else ul(items.map(item => li(valueOf(item))))

  private def isNumber(value: Json): Boolean = value match
    case _: Json.Num     => true
    case _: Json.Lexical => true
    case _               => false

  private def valueOf(value: Json): Frag = term(value) match
    case Some(frag) => frag
    case None =>
      value match
        case Json.Obj(fields) => dl(fields.map((name, nested) => frag(dt(name), dd(valueOf(nested)))))
        case Json.Arr(items)  => if items.isEmpty then span("—") else table(items)
        case Json.Str(text)   => if text.startsWith("/") || text.startsWith("?") then a(href := text, text) else span(text)
        case other            => span(plain(other))

  /** A term object, rendered as the thing it names */
  private def term(value: Json): Option[Frag] = value match
    case Json.Obj(fields) =>
      val lookup = fields.toMap
      (lookup.get("type"), lookup.get("value")) match
        case (Some(Json.Str("iri")), Some(Json.Str(iri))) =>
          Some(a(href := s"${Service.link(Service.operationPath("describe"))}?iri=${encode(s"<$iri>")}", abbreviate(iri)))
        case (Some(Json.Str("literal")), Some(Json.Str(lexical))) =>
          val language = lookup.get("lang").collect { case Json.Str(tag) => s"@$tag" }
          val datatype = lookup.get("datatype").collect { case Json.Str(iri) => s"^^${abbreviate(iri)}" }
          Some(frag(code(lexical), language.orElse(datatype).map(suffix => span(cls := "tag")(s" $suffix")).getOrElse(frag())))
        case (Some(Json.Str(kind)), Some(Json.Str(lexical))) => Some(frag(code(lexical), span(cls := "tag")(s" $kind")))
        case _                                               => None
    case _ => None

  /** An IRI under a declared prefix, written as a CURIE */
  private def abbreviate(iri: String): String =
    Service.Prefixes
      .filter((_, namespace) => iri.startsWith(namespace))
      .sortBy((_, namespace) => -namespace.length)
      .headOption
      .map((prefix, namespace) => s"$prefix:${iri.drop(namespace.length)}")
      .getOrElse(iri)

  private def plain(value: Json): String = value match
    case Json.Str(text)     => text
    case Json.Num(number)   => number.toString
    case Json.Lexical(text) => text
    case Json.Bool(flag)    => flag.toString
    case Json.Null          => "—"
    case other              => Json.write(other)

  /** A text document, such as the Markdown summary card */
  def document(heading: String, text: String): String = page(heading, pre(text))
