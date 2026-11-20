package org.renci.frink.kgf

import org.apache.jena.graph.Graph
import org.apache.jena.graph.Triple
import org.apache.jena.riot.RDFDataMgr
import org.apache.jena.riot.RDFFormat
import org.apache.jena.shared.PrefixMapping
import org.apache.jena.shared.impl.PrefixMappingImpl
import org.apache.jena.sparql.core.DatasetGraph
import org.apache.jena.sparql.core.Quad
import org.apache.jena.sparql.core.mem.DatasetGraphInMemory
import org.apache.jena.sparql.graph.GraphFactory

import java.io.StringWriter
import scala.jdk.CollectionConverters.*

/** The RDF representations of an answer.
  *
  * Graph tagging follows `docs/graph-semantics.md`: a scoped request tags every statement with the graph it named, the quad view tags per
  * row, and the union is written untagged, in the document's default graph. A single-graph syntax can carry the union and a scoped view,
  * and refuses the quad view.
  */
object Rdf:

  val prefixes: PrefixMapping = PrefixMappingImpl().setNsPrefixes(Service.Prefixes.toMap.asJava)

  def quads(rows: Seq[Quad], format: Format, quadView: Boolean): Either[Problem, String] =
    format match
      case Format.Turtle =>
        if quadView then
          Left(
            Problem(
              Code.NotAcceptable,
              "the quad view has a graph per row, which Turtle cannot carry; ask for nq, trig or jsonld, or scope the request with g="
            )
          )
        else Right(write(graph(rows), RDFFormat.TURTLE))
      case Format.NQuads => Right(write(dataset(rows), RDFFormat.NQUADS))
      case Format.TriG   => Right(write(dataset(rows), RDFFormat.TRIG))
      case Format.JsonLd => Right(write(dataset(rows), RDFFormat.JSONLD))
      case other         => Left(Problem(Code.UnsupportedFormat, s"${other.name} is not an RDF serialization"))

  /** The union written as triples in the default graph, every other scope tagged with its graph */
  private def dataset(rows: Seq[Quad]): DatasetGraph =
    val data = DatasetGraphInMemory()
    data.prefixes().putAll(prefixes)
    rows.foreach { quad =>
      if Term.isUnion(quad.getGraph()) then data.getDefaultGraph().add(quad.asTriple()) else data.add(quad)
    }
    data

  private def graph(rows: Seq[Quad]): Graph =
    val data = GraphFactory.createDefaultGraph()
    data.getPrefixMapping().setNsPrefixes(prefixes)
    rows.foreach(quad => data.add(quad.asTriple()))
    data

  def write(data: DatasetGraph, format: RDFFormat): String =
    val writer = StringWriter()
    RDFDataMgr.write(writer, data, format)
    writer.toString

  def write(data: Graph, format: RDFFormat): String =
    val writer = StringWriter()
    RDFDataMgr.write(writer, data, format)
    writer.toString

  /** A graph built from triples, with this dataset's prefixes */
  def build(triples: Seq[Triple], extraPrefixes: Map[String, String] = Map.empty): Graph =
    val data = GraphFactory.createDefaultGraph()
    data.getPrefixMapping().setNsPrefixes(prefixes)
    extraPrefixes.foreach((prefix, namespace) => data.getPrefixMapping().setNsPrefix(prefix, namespace))
    triples.foreach(data.add)
    data
