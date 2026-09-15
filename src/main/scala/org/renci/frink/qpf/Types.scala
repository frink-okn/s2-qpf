package org.renci.frink.qpf

import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.apache.jena.riot.RDFDataMgr
import org.apache.jena.riot.RDFFormat
import org.apache.jena.riot.writer.NQuadsWriter
import org.apache.jena.riot.writer.TriGWriterFlat
import org.apache.jena.sparql.core.DatasetGraph
import org.apache.jena.sparql.core.Quad
import sttp.model.MediaType
import sttp.tapir.*
import sttp.tapir.CodecFormat.TextHtml
import sttp.tapir.CodecFormat.TextPlain

import java.io.StringWriter
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Types:

  enum Term:
    case Variable(name: String)
    case IRI(iri: String)
    case Literal(lexicalForm: String, datatype: Option[String], language: Option[String])

    def isBlank: Boolean = this match
      case Variable(name) => name.isBlank()
      case IRI(iri)       => iri.isBlank()
      case _: Literal     => false

  object Term:
    def parse(value: String): Try[Term] =
      if value.startsWith("?") then Success(Variable(value.drop(1)))
      else if value.startsWith("\"") then parseLiteral(value)
      else if value.startsWith("_") then Failure(Exception("Blank nodes are not allowed"))
      else Success(IRI(value))

    /** Hydra explicit representation: `"lexical form"`, `"lexical form"@language`, or `"lexical form"^^datatype` */
    private def parseLiteral(value: String): Try[Term] =
      val closingQuote = value.lastIndexOf('"')
      if closingQuote < 1 then Failure(Exception(s"Invalid literal: $value"))
      else
        val lexicalForm = value.substring(1, closingQuote)
        val suffix = value.substring(closingQuote + 1)
        if suffix.isEmpty then Success(Literal(lexicalForm, None, None))
        else if suffix.startsWith("@") && suffix.length > 1 then Success(Literal(lexicalForm, None, Some(suffix.drop(1))))
        else if suffix.startsWith("^^") && suffix.length > 2 then
          Success(Literal(lexicalForm, Some(suffix.drop(2).stripPrefix("<").stripSuffix(">")), None))
        else Failure(Exception(s"Invalid literal: $value"))

    def decode(s: String): DecodeResult[Term] = Term.parse(s) match
      case Success(v) => DecodeResult.Value(v)
      case Failure(f) => DecodeResult.Error(s, f)

    def encode(term: Term): String = term match
      case Variable(name)                             => s"?$name"
      case IRI(iri)                                   => iri
      case Literal(lexicalForm, _, Some(language))    => s"\"$lexicalForm\"@$language"
      case Literal(lexicalForm, Some(datatype), None) => s"\"$lexicalForm\"^^$datatype"
      case Literal(lexicalForm, None, None)           => s"\"$lexicalForm\""

    def toNode(term: Term): Node = term match
      case Variable(name)                          => NodeFactory.createVariable(name)
      case IRI(iri)                                => NodeFactory.createURI(iri)
      case Literal(lexicalForm, _, Some(language)) => NodeFactory.createLiteralLang(lexicalForm, language)
      case Literal(lexicalForm, Some(datatype), None) =>
        NodeFactory.createLiteralDT(lexicalForm, TypeMapper.getInstance().getSafeTypeByName(datatype))
      case Literal(lexicalForm, None, None) => NodeFactory.createLiteralString(lexicalForm)

    def fromNode(node: Node): Term =
      if node.isVariable() then Variable(node.getName())
      else if node.isLiteral() then
        val language = Option(node.getLiteralLanguage()).filter(_.nonEmpty)
        val datatype = Option(node.getLiteralDatatypeURI()).filter(_ => language.isEmpty).filterNot(_ == XSDDatatype.XSDstring.getURI())
        Literal(node.getLiteralLexicalForm(), datatype, language)
      else IRI(node.getURI())

    val notLiteral: Validator[Term] =
      Validator.custom(term => ValidationResult.validWhen(!term.isInstanceOf[Literal]), Some("Literals are only allowed as the object"))

    given Codec[String, Term, TextPlain] = Codec.string.mapDecode(decode)(encode)

  object DatasetGraphUtils:
    def fromTrig(text: String): DecodeResult[QuadPatternFragment] = ???
    def fromNQuads(text: String): DecodeResult[QuadPatternFragment] = ???
    def fromJSONLD(text: String): DecodeResult[QuadPatternFragment] = ???
    def fromHTML(text: String): DecodeResult[QuadPatternFragment] = ???

    def toTrig(qpf: QuadPatternFragment): String =
      val writer = StringWriter()
      RDFDataMgr.write(writer, qpf.toDatasetGraph, RDFFormat.TRIG)
      writer.toString()

    def toNQuads(qpf: QuadPatternFragment): String =
      val writer = StringWriter()
      RDFDataMgr.write(writer, qpf.toDatasetGraph, RDFFormat.NQUADS)
      writer.toString()

    def toJSONLD(qpf: QuadPatternFragment): String =
      val writer = StringWriter()
      RDFDataMgr.write(writer, qpf.toDatasetGraph, RDFFormat.JSONLD)
      writer.toString()

    def toHTML(qpf: QuadPatternFragment): String = WebUI.toHTML(qpf)

    given trigFormat: Codec[String, QuadPatternFragment, ApplicationTrig] =
      Codec.string.mapDecode(fromTrig)(toTrig).format(ApplicationTrig())

    given nQuadsFormat: Codec[String, QuadPatternFragment, ApplicationNQuads] =
      Codec.string.mapDecode(fromNQuads)(toNQuads).format(ApplicationNQuads())

    given jsonldFormat: Codec[String, QuadPatternFragment, ApplicationJSONLD] =
      Codec.string.mapDecode(fromJSONLD)(toJSONLD).format(ApplicationJSONLD())

    given htmlFormat: Codec[String, QuadPatternFragment, TextHtml] =
      Codec.string.mapDecode(fromHTML)(toHTML).format(TextHtml())

  case class ApplicationTrig() extends CodecFormat:
    override val mediaType: MediaType = MediaType("application", "trig", Some("utf-8"))

  case class ApplicationNQuads() extends CodecFormat:
    override val mediaType: MediaType = MediaType("application", "n-quads", Some("utf-8"))

  case class ApplicationJSONLD() extends CodecFormat:
    override val mediaType: MediaType = MediaType("application", "ld+json", Some("utf-8"))
