package org.renci.frink.qpf

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

  enum VariableOrIRI(val value: String):
    case Variable(override val value: String) extends VariableOrIRI(value)
    case IRI(override val value: String) extends VariableOrIRI(value)

  object VariableOrIRI:
    def parse(value: String): Try[VariableOrIRI] =
      if value.startsWith("?") then Success(Variable(value.drop(1)))
      else if value.startsWith("\"") then Failure(Exception("Literals are not allowed"))
      else if value.startsWith("_") then Failure(Exception("Blank nodes are not allowed"))
      else Success(IRI(value))

    def decode(s: String): DecodeResult[VariableOrIRI] = VariableOrIRI.parse(s) match
      case Success(v) => DecodeResult.Value(v)
      case Failure(f) => DecodeResult.Error(s, f)

    def encode(term: VariableOrIRI): String = term match
      case Variable(value) => s"?$value"
      case IRI(value)      => value

    given Codec[String, VariableOrIRI, TextPlain] = Codec.string.mapDecode(decode)(encode)

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
