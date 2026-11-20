package org.renci.frink.kgf

import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.apache.jena.sparql.core.Quad
import org.apache.jena.vocabulary.RDF.Nodes as RDF
import org.apache.jena.vocabulary.RDFS.Nodes as RDFS
import org.renci.frink.s2.S2Graph

/** The statistics `/manifest`, `/void`, `/schema` and `/summary` all read, in closed form.
  *
  * Every number here is the size of the pattern that answers the matching `/count`, so doc 04's invariant — the top-level VoID equals
  * `/count` — holds by construction rather than by a second counting path that could drift from the first. `ModelSpec` checks each
  * primitive against the pattern size it claims to equal.
  *
  * A view is a set of level graphs. Its subjects are not only the cells of those levels: a containment quad lives in the **child** cell's
  * level graph, so the graph of level 13 also has every ancestor of a level-13 cell as a subject. The model is therefore decomposed by
  * subject level, and a class partition is the sum over the subject levels that contribute to it.
  */
object Model:

  val XSDString = "http://www.w3.org/2001/XMLSchema#string"
  val XSDInteger = "http://www.w3.org/2001/XMLSchema#integer"
  val XSDDouble = "http://www.w3.org/2001/XMLSchema#double"
  val WKTLiteral = s"${S2Graph.GeoSPARQL}wktLiteral"

  val Levels: Seq[Int] = S2Graph.Levels.toVector

  /** Cells at a level: six cube faces, each divided into 4^level */
  def cells(level: Int): BigInt = BigInt(6) * BigInt(4).pow(level)

  /** Every cell above a level, which is the set of ancestors the cells at that level have between them */
  def ancestors(level: Int): BigInt = 2 * (BigInt(4).pow(level) - 1)

  /** Same-level neighbour quads at a level. Taken from the pattern that answers them, because the cells at the corners of a cube face have
    * seven neighbours instead of eight (four at level 0) and no multiple of the cell count states it.
    */
  def touches(level: Int): BigInt = touchCounts(level)

  private lazy val touchCounts: Vector[BigInt] = Levels.toVector.map(level => size(S2Graph.Graphs(level), S2Graph.TouchesNode))

  /** The quads of a level graph, from the pattern that answers them */
  def levelTriples(level: Int): BigInt = levelTripleCounts(level)

  private lazy val levelTripleCounts: Vector[BigInt] = Levels.toVector.map(level => size(S2Graph.Graphs(level), Term.freshVariable))

  private def size(graph: Node, predicate: Node): BigInt =
    val pattern = Quad.create(graph, Term.freshVariable, predicate, Term.freshVariable)
    S2Graph.quads(pattern).map(_.size).getOrElse(BigInt(0))

  val Predicates: Seq[Node] = Vector(
    RDF.`type`,
    RDFS.label,
    S2Graph.CellIDNode,
    S2Graph.HasGeometryNode,
    S2Graph.DefaultGeometryNode,
    S2Graph.HasMetricAreaNode,
    S2Graph.AsWKTNode,
    S2Graph.WithinNode,
    S2Graph.ContainsNode,
    S2Graph.TouchesNode,
    S2Graph.ConnectedToNode
  )

  /** The classes shared by every cell or geometry, and the per-level cell classes */
  val SharedClasses: Seq[Node] = Vector(S2Graph.S2CellClass, S2Graph.FeatureClass, S2Graph.SpatialObjectClass, S2Graph.GeometryClass)

  val LevelClasses: Seq[Node] = Levels.map(S2Graph.LevelClasses)

  val Classes: Seq[Node] = SharedClasses ++ LevelClasses

  /** One predicate's statistics within a partition. A statistic this server cannot state exactly is `None` and is left out of the response
    * rather than invented as zero (KGF doc 03 section 3.4.10): two cells can carry the same `geo:hasMetricArea` literal, and how many do is
    * not closed-form, so that partition states no distinct-object count.
    */
  final case class Property(
      predicate: Node,
      triples: BigInt,
      subjects: Option[BigInt] = None,
      objects: Option[BigInt] = None,
      datatype: Option[String] = None
  ):
    def plus(other: Property): Property = Property(
      predicate,
      triples + other.triples,
      combine(subjects, other.subjects),
      combine(objects, other.objects),
      datatype.orElse(other.datatype)
    )

    private def combine(a: Option[BigInt], b: Option[BigInt]): Option[BigInt] =
      if a.isEmpty && b.isEmpty then None else Some(a.getOrElse(BigInt(0)) + b.getOrElse(BigInt(0)))

  final case class ClassPartition(term: Node, entities: BigInt, properties: Seq[Property]):
    def triples: BigInt = properties.map(_.triples).sum

  /** An observed class relation: subjects typed A use P to objects typed B. Not an `rdf:type` edge and not a declared domain or range, and
    * these counts do not partition the triple count, because a cell is typed four ways at once.
    */
  final case class Relation(subjectClass: Node, predicate: Node, objectClass: Node, triples: BigInt)

  /** A set of level graphs described as one: the whole dataset, or one level */
  final case class View(name: String, levels: Set[Int]):

    /** Subject levels that contribute: the view's own levels, and every level above the deepest, whose cells are the subjects of the
      * containment quads that live in a deeper level's graph
      */
    val subjectLevels: Seq[Int] = if levels.isEmpty then Vector.empty else (levels ++ (0 until levels.max)).toVector.sorted

    def triples: BigInt = levels.toVector.map(levelTriples).sum

    def cellCount: BigInt = subjectLevels.map(level => cellEntities(level, levels)).sum

    def geometryCount: BigInt = levels.toVector.map(cells).sum

    /** Distinct subjects: every cell and every geometry this view mentions as a subject */
    def subjects: BigInt = cellCount + geometryCount

    private def cellProperties: Seq[Property] = merge(subjectLevels.flatMap(level => Model.cellProperties(level, levels)))

    private def geometryProperties: Seq[Property] = merge(levels.toVector.sorted.flatMap(Model.geometryProperties))

    /** The dataset-level property partitions: every triple of the view, by predicate */
    def properties: Seq[Property] = merge(cellProperties ++ geometryProperties)

    def classes: Seq[ClassPartition] =
      val cellProps = cellProperties
      val cellEntityCount = cellCount
      val geometryProps = geometryProperties
      val shared = Vector(
        ClassPartition(S2Graph.S2CellClass, cellEntityCount, cellProps),
        ClassPartition(S2Graph.FeatureClass, cellEntityCount, cellProps),
        ClassPartition(S2Graph.GeometryClass, geometryCount, geometryProps),
        ClassPartition(S2Graph.SpatialObjectClass, cellEntityCount + geometryCount, merge(cellProps ++ geometryProps))
      )
      val perLevel = subjectLevels.map { level =>
        ClassPartition(S2Graph.LevelClasses(level), cellEntities(level, levels), merge(Model.cellProperties(level, levels)))
      }
      (shared ++ perLevel).filter(_.entities > 0)

    def classPartition(term: Node): Option[ClassPartition] = classes.find(_.term == term)

    def property(predicate: Node): Option[Property] = properties.find(_.predicate == predicate)

    def propertyOf(classTerm: Node, predicate: Node): Option[Property] =
      classPartition(classTerm).flatMap(_.properties.find(_.predicate == predicate))

    /** Every class relation of this view, ordered as KGF doc 03 section 3.4.10 orders the projection: by triples descending, then by the
      * expanded bytes of the subject class, the predicate and the object class
      */
    lazy val relations: Seq[Relation] = Model.relations(levels).sortBy { relation =>
      (-relation.triples, relation.subjectClass.getURI(), relation.predicate.getURI(), relation.objectClass.getURI())
    }

  /** The whole dataset: every level graph */
  val Queryable: View = View("queryable", Levels.toSet)

  /** No component is declared, so the design view describes the dataset itself under a second name (KGF doc 03 section 3.4.10) */
  val Design: View = View("design", Levels.toSet)

  private lazy val levelViews: Vector[View] = Levels.toVector.map(level => View(s"graph:${S2Graph.Graphs(level).getURI()}", Set(level)))

  def levelView(level: Int): View = levelViews(level)

  def view(name: String): Option[View] = name match
    case "queryable" => Some(Queryable)
    case "design"    => Some(Design)
    case graph if graph.startsWith("graph:") =>
      val iri = graph.drop("graph:".length)
      S2Graph.toS2Level(NodeFactory.createURI(iri)).map(levelView)
    case _ => None

  val ViewNames: Seq[String] = Vector("design", "queryable") ++ Levels.map(level => levelView(level).name)

  /** Cells at `level` that this view has as subjects: the level's own cells when the view holds that graph, and the same cells when a
    * deeper graph in the view holds containment quads pointing up at them
    */
  private def cellEntities(level: Int, levels: Set[Int]): BigInt =
    if levels.contains(level) || levels.exists(_ > level) then cells(level) else BigInt(0)

  /** Triples with a cell subject at `level`, within the graphs of `levels` */
  def cellProperties(level: Int, levels: Set[Int]): Seq[Property] =
    val own = levels.contains(level)
    val count = cells(level)
    val descendants = levels.toVector.filter(_ > level).map(cells).sum
    val above = ancestors(level)
    val within = if own then BigInt(level) * count else BigInt(0)
    val touching = if own then touches(level) else BigInt(0)
    val connected = within + descendants + touching
    val describing =
      if own then
        Vector(
          Property(RDF.`type`, 4 * count, Some(count), Some(BigInt(4))),
          Property(RDFS.label, count, Some(count), Some(count), Some(XSDString)),
          Property(S2Graph.CellIDNode, count, Some(count), Some(count), Some(XSDInteger)),
          Property(S2Graph.HasGeometryNode, count, Some(count), Some(count)),
          Property(S2Graph.DefaultGeometryNode, count, Some(count), Some(count)),
          Property(S2Graph.HasMetricAreaNode, count, Some(count), None, Some(XSDDouble))
        )
      else Vector.empty
    val relating = Vector(
      Option.when(within > 0)(Property(S2Graph.WithinNode, within, Some(count), Some(above))),
      Option.when(descendants > 0)(Property(S2Graph.ContainsNode, descendants, Some(count), Some(descendants))),
      Option.when(touching > 0)(Property(S2Graph.TouchesNode, touching, Some(count), Some(count))),
      Option.when(connected > 0)(
        Property(
          S2Graph.ConnectedToNode,
          connected,
          Some(count),
          Some(above * signum(within) + descendants + count * signum(touching))
        )
      )
    ).flatten
    describing ++ relating

  /** Triples with a geometry subject at `level` */
  def geometryProperties(level: Int): Seq[Property] =
    val count = cells(level)
    Vector(
      Property(RDF.`type`, 2 * count, Some(count), Some(BigInt(2))),
      Property(RDFS.label, count, Some(count), Some(count), Some(XSDString)),
      Property(S2Graph.AsWKTNode, count, Some(count), Some(count), Some(WKTLiteral))
    )

  private def signum(value: BigInt): BigInt = if value > 0 then 1 else 0

  private def merge(properties: Seq[Property]): Seq[Property] =
    val merged = properties.groupBy(_.predicate).view.mapValues(_.reduce(_.plus(_))).toMap
    Predicates.flatMap(merged.get)

  /** The classes a cell at a level belongs to, and a geometry's */
  private def cellClasses(level: Int): Seq[Node] =
    Vector(S2Graph.S2CellClass, S2Graph.LevelClasses(level), S2Graph.FeatureClass, S2Graph.SpatialObjectClass)

  private val geometryClasses: Seq[Node] = Vector(S2Graph.GeometryClass, S2Graph.SpatialObjectClass)

  /** Every class relation in the graphs of `levels`, before ordering.
    *
    * `rdf:type`'s objects are classes, which this server states nothing about, so its targets are untyped: KGF doc 03 section 3.4.10 omits
    * an untyped target partition from this projection, and the triples stay in the property partition's own counts.
    */
  def relations(levels: Set[Int]): Seq[Relation] =
    val geometryLinks = for
      level <- levels.toVector.sorted
      predicate <- Vector(S2Graph.HasGeometryNode, S2Graph.DefaultGeometryNode)
      subjectClass <- cellClasses(level)
      objectClass <- geometryClasses
    yield Relation(subjectClass, predicate, objectClass, cells(level))
    val containment = for
      level <- levels.toVector.sorted
      parent <- 0 until level
      (predicate, child, ancestor) <- Vector(
        (S2Graph.WithinNode, level, parent),
        (S2Graph.ContainsNode, parent, level)
      )
      subjectClass <- cellClasses(child)
      objectClass <- cellClasses(ancestor)
    yield Relation(subjectClass, predicate, objectClass, cells(level))
    val neighbours = for
      level <- levels.toVector.sorted
      subjectClass <- cellClasses(level)
      objectClass <- cellClasses(level)
    yield Relation(subjectClass, S2Graph.TouchesNode, objectClass, touches(level))
    val connected = (containment ++ neighbours).map(relation => relation.copy(predicate = S2Graph.ConnectedToNode))
    sum(geometryLinks ++ containment ++ neighbours ++ connected)

  private def sum(relations: Seq[Relation]): Seq[Relation] =
    relations
      .groupBy(relation => (relation.subjectClass, relation.predicate, relation.objectClass))
      .toVector
      .map((key, group) => Relation(key._1, key._2, key._3, group.map(_.triples).sum))
