package org.renci.frink.kgf

import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory

import java.util.UUID

/** Term syntax in parameters and bindings bodies: KGF doc 03 section 3.3.
  *
  *   - IRIs in angle brackets, `<http://example.org/thing>`
  *   - CURIEs bare, against the manifest prefix map, `kwg-ont:sfTouches`
  *   - literals quoted, with an optional `@lang` or `^^datatype`
  *   - a variable as `?name`, and an omitted parameter as a variable too
  *   - blank nodes as `_:label`: parsed, and never matched
  *
  * Brackets are required rather than optional. A parameter that accepted an IRI and a CURIE with no delimiter would have to infer which it
  * was handed, which makes a term's meaning depend on the prefix map of the dataset it is sent to and turns a misspelled prefix into a URI
  * scheme. This is what Turtle and SPARQL do, and for the same reasons.
  */
object Term:

  val UnionIRI = "urn:x-kgf:union"

  val UnnamedIRI = "urn:x-kgf:unnamed"

  /** The quad view, which is not a term: requiring brackets around IRIs is what keeps this value from ever colliding with one */
  val QuadView = "*"

  def parse(text: String): Either[Problem, Node] =
    val value = text.trim
    if value.isEmpty then Left(Problem.badTerm("a term parameter is empty; omit it to leave the position unbound"))
    else if value.startsWith("<") then
      if value.endsWith(">") && value.length > 2 then Right(NodeFactory.createURI(value.drop(1).dropRight(1)))
      else Left(Problem.badTerm(s"$value is not a term: an IRI in angle brackets must close them"))
    else if value.startsWith("\"") then literal(value)
    else if value.startsWith("?") then
      if value.length > 1 then Right(NodeFactory.createVariable(value.drop(1)))
      else Left(Problem.badTerm("?  is not a term: a variable needs a name"))
    else if value.startsWith("_:") then Right(NodeFactory.createBlankNode(value.drop(2)))
    else curie(value)

  /** A fresh variable for an omitted position. The name is unique so that two omitted positions are never read as a join: no quad of this
    * graph has one term in two positions, so a repeated variable matches nothing.
    */
  def freshVariable: Node = NodeFactory.createVariable(UUID.randomUUID().toString)

  def isUnion(node: Node): Boolean = org.renci.frink.qpf.QuadPatternFragment.UnionGraphs(node)

  private def literal(value: String): Either[Problem, Node] =
    val closingQuote = value.lastIndexOf('"')
    if closingQuote < 1 then Left(Problem.badTerm(s"$value is not a term: a literal must close its quotes"))
    else
      val lexicalForm = value.substring(1, closingQuote)
      val suffix = value.substring(closingQuote + 1)
      if suffix.isEmpty then Right(NodeFactory.createLiteralString(lexicalForm))
      else if suffix.startsWith("@") && suffix.length > 1 then Right(NodeFactory.createLiteralLang(lexicalForm, suffix.drop(1)))
      else if suffix.startsWith("^^") && suffix.length > 2 then
        datatype(suffix.drop(2)).map(iri => NodeFactory.createLiteralDT(lexicalForm, TypeMapper.getInstance().getSafeTypeByName(iri)))
      else Left(Problem.badTerm(s"$value is not a term: a literal takes an optional @language or ^^datatype after its quotes"))

  /** The datatype after `^^`, an IRI or a CURIE under the same two rules as any other term */
  def datatype(text: String): Either[Problem, String] =
    val value = text.trim
    if value.startsWith("<") && value.endsWith(">") && value.length > 2 then Right(value.drop(1).dropRight(1))
    else curie(value).map(_.getURI())

  private def curie(value: String): Either[Problem, Node] =
    value.split(":", 2) match
      case Array(prefix, local) =>
        Service.PrefixMap.get(prefix) match
          case Some(namespace) => Right(NodeFactory.createURI(s"$namespace$local"))
          case None =>
            Left(
              Problem.badTerm(
                s"$value is not a term: the prefix $prefix is not declared. Write an IRI in angle brackets, or use a prefix the " +
                  s"manifest declares: ${Service.Prefixes.map(_._1).mkString(", ")}"
              )
            )
      case _ =>
        Left(Problem.badTerm(s"$value is not a term: write an IRI in angle brackets, a declared CURIE, a quoted literal, or ?variable"))

  /** The spelling a response echoes for a bound position. An IRI is written back in angle brackets, which is the form a client can resend.
    */
  def encode(node: Node): String =
    if node.isURI() then s"<${node.getURI()}>"
    else if node.isVariable() then s"?${node.getName()}"
    else if node.isBlank() then s"_:${node.getBlankNodeLabel()}"
    else
      val language = Option(node.getLiteralLanguage()).filter(_.nonEmpty)
      val datatypeIRI = Option(node.getLiteralDatatypeURI()).filter(_ => language.isEmpty)
      val lexicalForm = s""""${node.getLiteralLexicalForm()}""""
      language.map(tag => s"$lexicalForm@$tag").orElse(datatypeIRI.map(iri => s"$lexicalForm^^<$iri>")).getOrElse(lexicalForm)
