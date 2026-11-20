package org.renci.frink.kgf

import org.apache.jena.graph.Node

/** A JSON value, written by `Json.write`.
  *
  * A count here can be any size: this dataset holds 1,361,984,604,108,888,559,446 triples, so counts pass 2^64 and `Num` carries a `BigInt`
  * written in full digits, never in exponent form (KGF doc 03 section 3.4.4).
  */
enum Json:
  case Str(value: String)
  case Num(value: BigInt)

  /** A number whose lexical form is fixed by its source, such as a cell's area, written exactly as given */
  case Lexical(value: String)
  case Bool(value: Boolean)
  case Null
  case Arr(items: Seq[Json])
  case Obj(fields: Seq[(String, Json)])

object Json:

  def obj(fields: (String, Json)*): Json = Obj(fields.toVector)

  def arr(items: Json*): Json = Arr(items.toVector)

  def int(value: Int): Json = Num(BigInt(value))

  def int(value: Long): Json = Num(BigInt(value))

  /** Fields whose value is absent are left out, which is how an optional member of a response envelope is omitted rather than sent null */
  def fields(entries: (String, Option[Json])*): Seq[(String, Json)] = entries.toVector.collect { case (name, Some(value)) => name -> value }

  def write(json: Json): String =
    val out = StringBuilder()
    writeTo(out, json, 0)
    out.result()

  private def writeTo(out: StringBuilder, json: Json, depth: Int): Unit =
    val pad = "  " * depth
    val innerPad = "  " * (depth + 1)
    json match
      case Str(value)     => out ++= quote(value)
      case Num(value)     => out ++= value.toString
      case Lexical(value) => out ++= value
      case Bool(value)    => out ++= (if value then "true" else "false")
      case Null           => out ++= "null"
      case Arr(items) =>
        if items.isEmpty then out ++= "[]"
        else
          out ++= "[\n"
          items.zipWithIndex.foreach { (item, index) =>
            out ++= innerPad
            writeTo(out, item, depth + 1)
            if index < items.size - 1 then out ++= ","
            out ++= "\n"
          }
          out ++= pad ++= "]"
      case Obj(entries) =>
        if entries.isEmpty then out ++= "{}"
        else
          out ++= "{\n"
          entries.zipWithIndex.foreach { case ((name, value), index) =>
            out ++= innerPad ++= quote(name) ++= ": "
            writeTo(out, value, depth + 1)
            if index < entries.size - 1 then out ++= ","
            out ++= "\n"
          }
          out ++= pad ++= "}"

  private def quote(text: String): String =
    val out = StringBuilder("\"")
    text.foreach {
      case '"'                          => out ++= "\\\""
      case '\\'                         => out ++= "\\\\"
      case '\n'                         => out ++= "\\n"
      case '\r'                         => out ++= "\\r"
      case '\t'                         => out ++= "\\t"
      case '\b'                         => out ++= "\\b"
      case '\f'                         => out ++= "\\f"
      case character if character < ' ' => out ++= f"\\u${character.toInt}%04x"
      case character                    => out += character
    }
    out ++= "\""
    out.result()

  /** A term as SPARQL Results JSON encodes it, so existing tooling parses a row unchanged (KGF doc 03 section 3.4.1) */
  def term(node: Node): Json =
    if node.isURI() then obj("type" -> Str("iri"), "value" -> Str(node.getURI()))
    else if node.isLiteral() then
      val language = Option(node.getLiteralLanguage()).filter(_.nonEmpty)
      val datatype = Option(node.getLiteralDatatypeURI())
        .filter(_ => language.isEmpty)
        .filterNot(_ == "http://www.w3.org/2001/XMLSchema#string")
      Obj(
        Vector("type" -> Str("literal"), "value" -> Str(node.getLiteralLexicalForm())) ++
          language.map("lang" -> Str(_)) ++ datatype.map("datatype" -> Str(_))
      )
    else if node.isVariable() then obj("type" -> Str("variable"), "value" -> Str(node.getName()))
    else obj("type" -> Str("bnode"), "value" -> Str(node.getBlankNodeLabel()))
