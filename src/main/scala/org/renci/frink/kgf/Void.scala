package org.renci.frink.kgf

import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.apache.jena.graph.Triple
import org.apache.jena.riot.RDFFormat
import org.apache.jena.vocabulary.RDF.Nodes as RDF
import org.renci.frink.s2.S2Graph

/** The VoID description, serialized per request from the closed-form model (KGF doc 03 section 3.4.10).
  *
  * What it carries: the dataset with its totals, the whole class and property tree of the queryable view, and one `void:subset` per level
  * graph with that graph's totals and its property partitions. It stops short of the class tree of each of the 31 level graphs, which would
  * be some twelve thousand partitions and several megabytes: `/schema?view=graph:<IRI>` is the drill-down for that, which is what `/schema`
  * exists for. `docs/kgf.md` section 12 records the choice.
  */
object Void:

  private val void = "http://rdfs.org/ns/void#"
  private val dct = "http://purl.org/dc/terms/"
  private val sd = "http://www.w3.org/ns/sparql-service-description#"
  private val foaf = "http://xmlns.com/foaf/0.1/"

  private def iri(value: String) = NodeFactory.createURI(value)

  private val Dataset = iri(s"${void}Dataset")
  private val Triples = iri(s"${void}triples")
  private val Entities = iri(s"${void}entities")
  private val Classes = iri(s"${void}classes")
  private val Properties = iri(s"${void}properties")
  private val DistinctSubjects = iri(s"${void}distinctSubjects")
  private val DistinctObjects = iri(s"${void}distinctObjects")
  private val ClassPartition = iri(s"${void}classPartition")
  private val PropertyPartition = iri(s"${void}propertyPartition")
  private val Class = iri(s"${void}class")
  private val Property = iri(s"${void}property")
  private val Subset = iri(s"${void}subset")
  private val UriSpace = iri(s"${void}uriSpace")
  private val Datatype = iri(s"${void}datatype")
  private val Title = iri(s"${dct}title")
  private val Description = iri(s"${dct}description")
  private val License = iri(s"${dct}license")
  private val Homepage = iri(s"${foaf}homepage")
  private val Graph = iri(s"${sd}graph")

  /** This document's own vocabulary, beside the dataset's published prefix map, which `Rdf.build` declares */
  private val extraPrefixes = Map("dct" -> dct, "sd" -> sd, "foaf" -> foaf)

  private def number(value: BigInt): Node = NodeFactory.createLiteralDT(value.toString, XSDDatatype.XSDinteger)

  def document(format: Format): Either[Problem, String] =
    val rdfFormat = format match
      case Format.Turtle => Right(RDFFormat.TURTLE)
      case Format.JsonLd => Right(RDFFormat.JSONLD)
      case other         => Left(Problem(Code.UnsupportedFormat, s"${other.name} is not an RDF serialization"))
    rdfFormat.map(chosen => Rdf.write(Rdf.build(triples, extraPrefixes), chosen))

  def triples: Seq[Triple] =
    val dataset = iri(Service.DatasetIRI)
    val view = Model.Queryable
    val identity = Vector(
      Triple.create(dataset, RDF.`type`, Dataset),
      Triple.create(dataset, Title, NodeFactory.createLiteralString(Service.Title)),
      Triple.create(dataset, Description, NodeFactory.createLiteralString(Service.Description)),
      Triple.create(dataset, License, iri(Service.License)),
      Triple.create(dataset, Homepage, iri(Service.Homepage)),
      Triple.create(dataset, UriSpace, NodeFactory.createLiteralString(s"${S2Graph.KWGResource}s2.")),
      Triple.create(dataset, UriSpace, NodeFactory.createLiteralString(s"${S2Graph.KWGResource}geometry.polygon.s2."))
    )
    identity ++ totals(dataset, view) ++
      view.classes.flatMap(partition => classPartition(dataset, partition)) ++
      view.properties.flatMap(property => propertyPartition(dataset, property)) ++
      Model.Levels.flatMap(subset(dataset, _))

  private def totals(subject: Node, view: Model.View): Seq[Triple] =
    Vector(
      Triple.create(subject, Triples, number(view.triples)),
      Triple.create(subject, DistinctSubjects, number(view.subjects)),
      Triple.create(subject, Properties, number(view.properties.size)),
      Triple.create(subject, Classes, number(view.classes.size))
    )

  private def classPartition(parent: Node, partition: Model.ClassPartition): Seq[Triple] =
    val node = NodeFactory.createBlankNode()
    Vector(
      Triple.create(parent, ClassPartition, node),
      Triple.create(node, Class, partition.term),
      Triple.create(node, Entities, number(partition.entities)),
      Triple.create(node, Triples, number(partition.triples))
    ) ++ partition.properties.flatMap(property => propertyPartition(node, property))

  /** A statistic the model cannot state exactly is left out rather than invented: two cells can carry the same area literal, and how many
    * do is not closed-form, so that partition states no distinct-object count (KGF doc 03 section 3.4.10).
    */
  private def propertyPartition(parent: Node, property: Model.Property): Seq[Triple] =
    val node = NodeFactory.createBlankNode()
    Vector(
      Triple.create(parent, PropertyPartition, node),
      Triple.create(node, Property, property.predicate),
      Triple.create(node, Triples, number(property.triples))
    ) ++ property.subjects.map(count => Triple.create(node, DistinctSubjects, number(count))) ++
      property.objects.map(count => Triple.create(node, DistinctObjects, number(count))) ++
      property.datatype.map(datatype => Triple.create(node, Datatype, iri(datatype)))

  /** One level graph, as a subset named by its service description */
  private def subset(parent: Node, level: Int): Seq[Triple] =
    val node = NodeFactory.createBlankNode()
    val view = Model.levelView(level)
    Vector(
      Triple.create(parent, Subset, node),
      Triple.create(node, RDF.`type`, Dataset),
      Triple.create(node, Graph, S2Graph.Graphs(level))
    ) ++ totals(node, view) ++ view.properties.flatMap(property => propertyPartition(node, property))
