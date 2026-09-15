package org.renci.frink.qpf

import org.apache.jena.datatypes.RDFDatatype
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.graph.NodeFactory
import org.apache.jena.shared.PrefixMapping
import org.apache.jena.shared.impl.PrefixMappingImpl
import org.apache.jena.sparql.core.DatasetGraph
import org.apache.jena.sparql.core.Quad
import org.apache.jena.sparql.core.mem.DatasetGraphInMemory
import org.apache.jena.sparql.vocabulary.FOAF
import org.apache.jena.vocabulary.DCTerms
import org.apache.jena.vocabulary.RDF.Nodes as RDF
import org.renci.frink.Util.SizedIterator
import org.renci.frink.qpf.Types.Term
import sttp.model.Uri
import sttp.tapir.*

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*

final case class FragmentMetadata(
    params: QuadPatternFragment.Parameters,
    itemsPerPage: Int,
    totalItems: BigInt,
    exactTotal: Boolean,
    endpoint: Uri,
    requestQuery: Option[String],
    customPrefixes: Map[String, String]
):
  private val voidDataset = NodeFactory.createURI("http://rdfs.org/ns/void#Dataset")
  private val voidSubset = NodeFactory.createURI("http://rdfs.org/ns/void#subset")
  private val voidTriples = NodeFactory.createURI("http://rdfs.org/ns/void#triples")
  private val hydraCollection = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#Collection")
  private val hydraSearch = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#search")
  private val hydraTemplate = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#template")
  private val hydraVariableRepresentation = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#variableRepresentation")
  private val hydraExplicitRepresentation = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#ExplicitRepresentation")
  private val hydraMapping = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#mapping")
  private val hydraVariable = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#variable")
  private val hydraProperty = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#property")
  private val hydraPartialCollectionView = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#PartialCollectionView")
  private val hydraTotalItems = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#totalItems")
  private val hydraItemsPerPage = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#itemsPerPage")
  private val hydraFirst = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#first")
  private val hydraNext = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#next")
  private val hydraPrevious = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#previous")
  private val hydraView = NodeFactory.createURI("http://www.w3.org/ns/hydra/core#view")
  private val sparqlGraph = NodeFactory.createURI("http://www.w3.org/ns/sparql-service-description#graph")
  private val sparqlDefaultDataset = NodeFactory.createURI("http://www.w3.org/ns/sparql-service-description#defaultDataset")
  private val sparqlDefaultGraph = NodeFactory.createURI("http://www.w3.org/ns/sparql-service-description#defaultGraph")
  private val defaultDatasetBNode = NodeFactory.createBlankNode("defaultDataset")
  private val patternBNode = NodeFactory.createBlankNode("pattern")
  private val subjectBNode = NodeFactory.createBlankNode("subject")
  private val predicateBNode = NodeFactory.createBlankNode("predicate")
  private val objectBNode = NodeFactory.createBlankNode("object")
  private val graphBNode = NodeFactory.createBlankNode("graph")

  val page = params.page.getOrElse(BigInt(1))
  val skip = (page - 1) * itemsPerPage
  val template = s"$endpoint{?subject,predicate,object,graph}"

  /** The requested URL, spelled as the client spelled it, since clients recognize the page's metadata by the URL they requested */
  val pageURL: String = endpoint.toString + requestQuery.map(query => s"?${FragmentMetadata.iriSafe(query)}").getOrElse("")
  private val fragmentQuery = requestQuery.toSeq
    .flatMap(query => FragmentMetadata.iriSafe(query).split('&'))
    .filterNot(parameter => URLDecoder.decode(parameter.takeWhile(_ != '='), StandardCharsets.UTF_8) == "page")
    .mkString("&")

  /** The URL of the fragment's first page: the requested URL without a page */
  val fragmentURL: String = if fragmentQuery.isEmpty then endpoint.toString else s"$endpoint?$fragmentQuery"
  def pageOf(number: BigInt): String = s"$fragmentURL${if fragmentQuery.isEmpty then "?" else "&"}page=$number"
  val previousPage: Option[String] = Option.when(page != 1)(pageOf(page - 1))
  val nextPage: Option[String] = Option.when(totalItems - skip - itemsPerPage > 0)(pageOf(page + 1))

  def toQuads: Set[Quad] =
    val totalItemsNode = NodeFactory.createLiteralDT(totalItems.toString(), XSDDatatype.XSDinteger)
    val itemsPerPageNode = NodeFactory.createLiteralDT(itemsPerPage.toString(), XSDDatatype.XSDinteger)
    val datasetNode = NodeFactory.createURI(endpoint.fragment("dataset").toString)
    val metadataGraphNode = NodeFactory.createURI(s"$fragmentURL#metadata")
    // Comunica takes the metadata graph to be the one about the resource with the requested page as a void:subset. Naming the fragment apart
    // from its first page keeps that resource unique, since the dataset's subset is then the fragment rather than the first page.
    val fragmentNode = NodeFactory.createURI(s"$fragmentURL#fragment")
    val firstPageNode = NodeFactory.createURI(fragmentURL)
    val pageNode = NodeFactory.createURI(pageURL)
    var quads = Set(
      Quad.create(metadataGraphNode, metadataGraphNode, FOAF.primaryTopic.asNode(), fragmentNode),
      Quad.create(metadataGraphNode, datasetNode, RDF.`type`, voidDataset),
      Quad.create(metadataGraphNode, datasetNode, RDF.`type`, hydraCollection),
      Quad.create(metadataGraphNode, datasetNode, voidSubset, fragmentNode),
      // Comunica only reads the default graph from a few subjects, blank nodes among them, and treats it as empty otherwise
      Quad.create(metadataGraphNode, datasetNode, sparqlDefaultDataset, defaultDatasetBNode),
      Quad.create(metadataGraphNode, defaultDatasetBNode, sparqlDefaultGraph, QuadPatternFragment.UnionGraph),
      Quad.create(metadataGraphNode, datasetNode, hydraSearch, patternBNode),
      Quad.create(metadataGraphNode, patternBNode, hydraTemplate, NodeFactory.createLiteralString(template)),
      Quad.create(metadataGraphNode, patternBNode, hydraVariableRepresentation, hydraExplicitRepresentation),
      Quad.create(metadataGraphNode, patternBNode, hydraMapping, subjectBNode),
      Quad.create(metadataGraphNode, patternBNode, hydraMapping, predicateBNode),
      Quad.create(metadataGraphNode, patternBNode, hydraMapping, objectBNode),
      Quad.create(metadataGraphNode, patternBNode, hydraMapping, graphBNode),
      Quad.create(metadataGraphNode, subjectBNode, hydraVariable, NodeFactory.createLiteralString("subject")),
      Quad.create(metadataGraphNode, subjectBNode, hydraProperty, RDF.subject),
      Quad.create(metadataGraphNode, predicateBNode, hydraVariable, NodeFactory.createLiteralString("predicate")),
      Quad.create(metadataGraphNode, predicateBNode, hydraProperty, RDF.predicate),
      Quad.create(metadataGraphNode, objectBNode, hydraVariable, NodeFactory.createLiteralString("object")),
      Quad.create(metadataGraphNode, objectBNode, hydraProperty, RDF.`object`),
      Quad.create(metadataGraphNode, graphBNode, hydraVariable, NodeFactory.createLiteralString("graph")),
      Quad.create(metadataGraphNode, graphBNode, hydraProperty, sparqlGraph),
      Quad.create(metadataGraphNode, fragmentNode, voidSubset, pageNode),
      Quad.create(metadataGraphNode, fragmentNode, hydraView, pageNode),
      Quad.create(metadataGraphNode, pageNode, RDF.`type`, hydraPartialCollectionView),
      Quad.create(metadataGraphNode, pageNode, DCTerms.title.asNode(), NodeFactory.createLiteralString("Linked Data Fragment")),
      Quad.create(metadataGraphNode, pageNode, DCTerms.source.asNode(), datasetNode),
      Quad.create(metadataGraphNode, pageNode, hydraTotalItems, totalItemsNode),
      Quad.create(metadataGraphNode, pageNode, voidTriples, totalItemsNode),
      Quad.create(metadataGraphNode, pageNode, hydraItemsPerPage, itemsPerPageNode),
      Quad.create(metadataGraphNode, pageNode, hydraFirst, firstPageNode)
    )
    previousPage.foreach(uri => quads += Quad.create(metadataGraphNode, pageNode, hydraPrevious, NodeFactory.createURI(uri)))
    nextPage.foreach(uri => quads += Quad.create(metadataGraphNode, pageNode, hydraNext, NodeFactory.createURI(uri)))
    quads
end FragmentMetadata

object FragmentMetadata:

  /** A URL's query with the characters that can't be in an IRI, or that a browser would encode, percent-encoded */
  def iriSafe(query: String): String =
    query
      .codePoints()
      .toArray()
      .map { codePoint =>
        if codePoint > ' ' && codePoint < 0x7f && !"\"#'<>\\^`{|}".contains(codePoint.toChar) then codePoint.toChar.toString
        else String(Character.toChars(codePoint)).getBytes(StandardCharsets.UTF_8).map(byte => f"%%${byte & 0xff}%02X").mkString
      }
      .mkString

/** The quads answering a request, by position in its pages. A position is empty when its quad is at an earlier position, so the number of
  * positions is only an upper bound on the number of quads, unless it is exact.
  */
final case class Matches(positions: SizedIterator[Option[Quad]], exact: Boolean)

object Matches:
  def of(quads: SizedIterator[Quad]): Matches = Matches(quads.map(Option(_)), true)

final case class QuadPatternFragment(data: Seq[Quad], metadata: FragmentMetadata):
  def toDatasetGraph: DatasetGraph =
    val dataset = DatasetGraphInMemory()
    dataset.prefixes().putAll(this.prefixMapping)
    data.foreach(dataset.add)
    metadata.toQuads.foreach(dataset.add)
    dataset

  def prefixMapping: PrefixMapping =
    PrefixMappingImpl().setNsPrefixes((QuadPatternFragment.qpfPrefixes ++ metadata.customPrefixes).asJava)

object QuadPatternFragment:

  /** Names the union of every graph, which is the default graph: a pattern outside any GRAPH matches each distinct triple once. The same
    * IRI names the union in KGF datasets.
    */
  val UnionGraph = NodeFactory.createURI("urn:x-kgf:union")

  val quadsPerPage = 100

  val qpfPrefixes = Map(
    "rdf" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs" -> "http://www.w3.org/2000/01/rdf-schema#",
    "owl" -> "http://www.w3.org/2002/07/owl#",
    "xsd" -> "http://www.w3.org/2001/XMLSchema#",
    "terms" -> "http://purl.org/dc/terms/",
    "hydra" -> "http://www.w3.org/ns/hydra/core#",
    "void" -> "http://rdfs.org/ns/void#"
  )

  final case class Parameters(
      s: Option[Term],
      p: Option[Term],
      o: Option[Term],
      g: Option[Term],
      page: Option[BigInt],
      values: Option[Bindings]
  )

  def qpf(
      matches: Matches,
      params: Parameters,
      customPrefixes: Map[String, String],
      endpoint: Uri,
      requestQuery: Option[String]
  ): QuadPatternFragment =
    val metadata =
      FragmentMetadata(params, quadsPerPage, matches.positions.size, matches.exact, endpoint, requestQuery, customPrefixes)
    val quads = matches.positions.drop(metadata.skip).iterator.take(quadsPerPage).flatten.toSeq
    QuadPatternFragment(quads, metadata)
