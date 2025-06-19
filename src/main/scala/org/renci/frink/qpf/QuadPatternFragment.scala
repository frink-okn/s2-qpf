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
import org.renci.frink.qpf.Types.VariableOrIRI
import sttp.model.Uri
import sttp.tapir.*

import scala.jdk.CollectionConverters.*

final case class FragmentMetadata(
    params: QuadPatternFragment.Parameters,
    itemsPerPage: Int,
    totalItems: BigInt,
    endpoint: Uri,
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
  private val sparqlDefaultGraph = NodeFactory.createURI("http://www.w3.org/ns/sparql-service-description#defaultGraph")
  private val ldfDefaultGraph = NodeFactory.createURI("urn:ldf:defaultGraph")
  private val patternBNode = NodeFactory.createBlankNode("pattern")
  private val subjectBNode = NodeFactory.createBlankNode("subject")
  private val predicateBNode = NodeFactory.createBlankNode("predicate")
  private val objectBNode = NodeFactory.createBlankNode("object")
  private val graphBNode = NodeFactory.createBlankNode("graph")

  private def toParamValue(term: VariableOrIRI): String =
    term match
      case VariableOrIRI.Variable(value) => s"?$value"
      case VariableOrIRI.IRI(value)      => value

  val page = params.page.getOrElse(BigInt(1))
  val skip = (page - 1) * itemsPerPage
  val template = s"$endpoint{?subject,predicate,object,graph}"
  val currentQPF = endpoint
    .addParam("subject", params.s.map(toParamValue))
    .addParam("predicate", params.p.map(toParamValue))
    .addParam("object", params.o.map(toParamValue))
    .addParam("graph", params.g.map(toParamValue))
  val currentQPFPage = currentQPF.addParam("page", params.page.map(_.toString))
  val previousQPFPage =
    if page != 1 then
      val previousPage = page - 1
      Some(currentQPF.addParam("page", previousPage.toString()))
    else None
  val nextQPFPage =
    if totalItems - skip - itemsPerPage > 0 then
      val nextPage = page + 1
      Some(currentQPF.addParam("page", nextPage.toString()))
    else None

  def toQuads: Set[Quad] =
    val totalItemsNode = NodeFactory.createLiteralDT(totalItems.toString(), XSDDatatype.XSDinteger)
    val itemsPerPageNode = NodeFactory.createLiteralDT(itemsPerPage.toString(), XSDDatatype.XSDinteger)
    val datasetNode = NodeFactory.createURI(endpoint.fragment("dataset").toString)
    val metadataGraphNode = NodeFactory.createURI(currentQPF.fragment("metadata").toString)
    val currentQPFNode = NodeFactory.createURI(currentQPF.toString)
    val currentQPFPageNode = NodeFactory.createURI(currentQPFPage.toString)
    var quads = Set(
      Quad.create(metadataGraphNode, metadataGraphNode, FOAF.primaryTopic.asNode(), currentQPFNode),
      Quad.create(metadataGraphNode, datasetNode, RDF.`type`, voidDataset),
      Quad.create(metadataGraphNode, datasetNode, RDF.`type`, hydraCollection),
      Quad.create(metadataGraphNode, datasetNode, voidSubset, currentQPFNode),
      Quad.create(metadataGraphNode, datasetNode, voidSubset, currentQPFPageNode),
      Quad.create(metadataGraphNode, datasetNode, sparqlDefaultGraph, ldfDefaultGraph),
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
      Quad.create(metadataGraphNode, currentQPFNode, voidSubset, currentQPFPageNode),
      Quad.create(metadataGraphNode, currentQPFNode, hydraView, currentQPFPageNode),
      Quad.create(metadataGraphNode, currentQPFPageNode, RDF.`type`, hydraPartialCollectionView),
      Quad.create(metadataGraphNode, currentQPFPageNode, DCTerms.title.asNode(), NodeFactory.createLiteralString("Linked Data Fragment")),
      Quad.create(metadataGraphNode, currentQPFPageNode, DCTerms.source.asNode(), datasetNode),
      Quad.create(metadataGraphNode, currentQPFPageNode, hydraTotalItems, totalItemsNode),
      Quad.create(metadataGraphNode, currentQPFPageNode, voidTriples, totalItemsNode),
      Quad.create(metadataGraphNode, currentQPFPageNode, hydraItemsPerPage, itemsPerPageNode),
      Quad.create(metadataGraphNode, currentQPFPageNode, hydraFirst, currentQPFNode)
    )
    previousQPFPage.foreach(uri =>
      quads += Quad.create(metadataGraphNode, currentQPFPageNode, hydraPrevious, NodeFactory.createURI(uri.toString))
    )
    nextQPFPage.foreach(uri => quads += Quad.create(metadataGraphNode, currentQPFPageNode, hydraNext, NodeFactory.createURI(uri.toString)))
    quads
end FragmentMetadata

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
      s: Option[VariableOrIRI],
      p: Option[VariableOrIRI],
      o: Option[VariableOrIRI],
      g: Option[VariableOrIRI],
      page: Option[BigInt]
  )

  def qpf(quadsIterator: SizedIterator[Quad], params: Parameters, customPrefixes: Map[String, String], endpoint: Uri): QuadPatternFragment =
    val metadata = FragmentMetadata(params, quadsPerPage, quadsIterator.size, endpoint, customPrefixes)
    val quads = quadsIterator.drop(metadata.skip).iterator.take(quadsPerPage).toSeq
    QuadPatternFragment(quads, metadata)
