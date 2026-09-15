package org.renci.frink.s2

import com.google.common.geometry.S2Cell
import com.google.common.geometry.S2CellId
import com.google.common.geometry.S2LatLng
import com.google.common.geometry.S2Projections
import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.apache.jena.sparql.core.Quad
import org.apache.jena.vocabulary.RDF.Nodes as RDF
import org.apache.jena.vocabulary.RDFS.Nodes as RDFS
import org.renci.frink.Util.FanOutIterator
import org.renci.frink.Util.MultiSizedIterator
import org.renci.frink.Util.SizedIterator

import java.lang.Long
import scala.jdk.CollectionConverters.*
import scala.util.Try

object S2Graph:
  val KWGResource = "http://stko-kwg.geog.ucsb.edu/lod/resource/"
  val S2Prefix = s"${KWGResource}s2.level"
  val GeometryPrefix = s"${KWGResource}geometry.polygon.s2.level"
  val KWGOnt = "http://stko-kwg.geog.ucsb.edu/lod/ontology/"
  val GeoSPARQL = "http://www.opengis.net/ont/geosparql#"
  val SpatialFull = "http://purl.org/spatialai/spatial/spatial-full#"
  val Within = s"${KWGOnt}sfWithin"
  val Contains = s"${KWGOnt}sfContains"
  val Touches = s"${KWGOnt}sfTouches"
  val ConnectedTo = s"${SpatialFull}connectedTo"
  val ContainsNode = NodeFactory.createURI(Contains)
  val WithinNode = NodeFactory.createURI(Within)
  val TouchesNode = NodeFactory.createURI(Touches)
  val ConnectedToNode = NodeFactory.createURI(ConnectedTo)
  val CellIDNode = NodeFactory.createURI(s"${KWGOnt}cellID")
  val HasGeometryNode = NodeFactory.createURI(s"${GeoSPARQL}hasGeometry")
  val DefaultGeometryNode = NodeFactory.createURI(s"${GeoSPARQL}defaultGeometry")
  val HasMetricAreaNode = NodeFactory.createURI(s"${GeoSPARQL}hasMetricArea")
  val AsWKTNode = NodeFactory.createURI(s"${GeoSPARQL}asWKT")
  val S2CellClass = NodeFactory.createURI(s"${KWGOnt}S2Cell")
  val FeatureClass = NodeFactory.createURI(s"${GeoSPARQL}Feature")
  val SpatialObjectClass = NodeFactory.createURI(s"${GeoSPARQL}SpatialObject")
  val GeometryClass = NodeFactory.createURI(s"${GeoSPARQL}Geometry")
  val WKTLiteral = TypeMapper.getInstance().getSafeTypeByName(s"${GeoSPARQL}wktLiteral")
  val Levels = 0 to S2CellId.MAX_LEVEL
  val Graphs = Levels.map(level => level -> NodeFactory.createURI(s"$S2Prefix$level")).toMap
  val LevelClasses = Levels.map(level => level -> NodeFactory.createURI(s"${KWGOnt}S2Cell_Level$level")).toMap

  /** The only objects shared by the descriptions of many cells or geometries */
  val ClassNodes: Set[Node] = Set(S2CellClass, FeatureClass, SpatialObjectClass, GeometryClass) ++ LevelClasses.values

  /** IAU nominal equatorial Earth radius, which SAWGraph's spatialkg uses for cell areas */
  val EarthRadiusMeters = 6378100.0

  final case class ContainmentRelation(parent: S2CellId, child: S2CellId)

  private enum CellPattern:
    case AnyCell
    case NoCell
    case OneCell(cell: S2CellId)

  import CellPattern.*

  def s2CellIRI(cell: S2CellId): String = s"$S2Prefix${cell.level()}.${Long.toUnsignedString(cell.id())}"

  def geometryIRI(cell: S2CellId): String = s"$GeometryPrefix${cell.level()}.${Long.toUnsignedString(cell.id())}"

  /** Quads matching a pattern, or the reason the pattern can't be answered */
  def quads(pattern: Quad): Either[String, SizedIterator[Quad]] =
    val subject = pattern.getSubject()
    val predicate = pattern.getPredicate()
    val obj = pattern.getObject()
    // nothing is related to itself
    if subject.matches(obj) then Right(SizedIterator.empty)
    else
      // Graphs other than the level graphs, such as the advertised default graph, match every level
      val levels = toS2Level(pattern.getGraph()).map(Seq(_)).getOrElse(Levels)
      def relation(relationPredicate: Node, relationQuads: => SizedIterator[Quad]): SizedIterator[Quad] =
        if matches(predicate, relationPredicate) then relationQuads else SizedIterator.empty
      descriptionQuads(subject, predicate, obj, levels).map(description =>
        MultiSizedIterator(
          Vector(
            description,
            relation(WithinNode, withinQuads(subject, obj, levels)),
            relation(ContainsNode, containsQuads(subject, obj, levels)),
            relation(TouchesNode, touchesQuads(subject, obj, levels)),
            relation(ConnectedToNode, connectedToQuads(subject, obj, levels))
          )
        )
      )

  /** Triples about a cell itself, rather than its relations to other cells */
  def cellDescription(cell: S2CellId): Seq[Quad] =
    val graph = Graphs(cell.level())
    val subject = NodeFactory.createURI(s2CellIRI(cell))
    val geometry = NodeFactory.createURI(geometryIRI(cell))
    val id = Long.toUnsignedString(cell.id())
    Seq(
      RDF.`type` -> S2CellClass,
      RDF.`type` -> LevelClasses(cell.level()),
      RDF.`type` -> FeatureClass,
      RDF.`type` -> SpatialObjectClass,
      RDFS.label -> NodeFactory.createLiteralString(s"S2 Cell at level ${cell.level()} with ID $id"),
      CellIDNode -> NodeFactory.createLiteralDT(id, XSDDatatype.XSDinteger),
      HasGeometryNode -> geometry,
      DefaultGeometryNode -> geometry,
      HasMetricAreaNode -> NodeFactory.createLiteralDT(area(cell).toString, XSDDatatype.XSDdouble)
    ).map((p, o) => Quad.create(graph, subject, p, o))

  /** Area in square meters */
  def area(cell: S2CellId): Double = S2Cell(cell).approxArea() * EarthRadiusMeters * EarthRadiusMeters

  /** Triples about the polygon geometry of a cell */
  def geometryDescription(cell: S2CellId): Seq[Quad] =
    val graph = Graphs(cell.level())
    val subject = NodeFactory.createURI(geometryIRI(cell))
    val id = Long.toUnsignedString(cell.id())
    Seq(
      RDF.`type` -> GeometryClass,
      RDF.`type` -> SpatialObjectClass,
      RDFS.label -> NodeFactory.createLiteralString(
        s"Geometry of the polygon formed from the vertices of the S2 Cell at level ${cell.level()} with ID $id"
      ),
      AsWKTNode -> NodeFactory.createLiteralDT(wkt(cell), WKTLiteral)
    ).map((p, o) => Quad.create(graph, subject, p, o))

  /** The cell's vertices as a closed longitude/latitude ring. This does not faithfully represent very large cells, or cells crossing the
    * antimeridian or containing a pole.
    */
  def wkt(cell: S2CellId): String =
    val s2Cell = S2Cell(cell)
    val vertices = (0 until 4).map(i => S2LatLng(s2Cell.getVertex(i)))
    (vertices :+ vertices.head)
      .map(vertex => s"${plainDecimal(vertex.lngDegrees())} ${plainDecimal(vertex.latDegrees())}")
      .mkString("POLYGON ((", ", ", "))")

  private def plainDecimal(value: Double): String = java.math.BigDecimal(value.toString).toPlainString()

  private def matches(pattern: Node, node: Node): Boolean = pattern.isVariable() || pattern == node

  private def descriptionQuads(subject: Node, predicate: Node, obj: Node, levels: Seq[Int]): Either[String, SizedIterator[Quad]] =
    def wanted(quad: Quad) = matches(predicate, quad.getPredicate()) && matches(obj, quad.getObject())
    def describe(cell: S2CellId) = cellDescription(cell) ++ geometryDescription(cell)
    if !subject.isVariable() then
      val cellQuads = toS2Cell(subject).filter(cell => levels.contains(cell.level())).map(cellDescription)
      val geometryQuads = toGeometryCell(subject).filter(cell => levels.contains(cell.level())).map(geometryDescription)
      Right(SizedIterator.fromSeq((cellQuads ++ geometryQuads).flatten.filter(wanted).toSeq))
    else if obj.isVariable() || ClassNodes(obj) then
      // Every cell at a level has the same number of matching triples, so pages can be skipped without enumerating cells
      Right(
        MultiSizedIterator(
          for
            level <- levels.toVector
            (description, sample) <- descriptions.zip(sampleDescriptions(level))
            fanOut = sample.count(wanted)
            if fanOut > 0
          yield FanOutIterator(cellsAt(level), fanOut, cell => description(cell).filter(wanted))
        )
      )
    else
      val cellsWithMetricArea = if matches(predicate, HasMetricAreaNode) then cellsWithArea(obj, levels) else Right(Seq.empty)
      cellsWithMetricArea.map { areaCells =>
        val candidates = (cellsNamedBy(obj) ++ areaCells).filter(cell => levels.contains(cell.level())).distinct
        SizedIterator.fromSeq(candidates.flatMap(cell => describe(cell).filter(wanted)))
      }

  private val descriptions: Vector[S2CellId => Seq[Quad]] = Vector(cellDescription, geometryDescription)

  /** For each level, the descriptions of its first cell. Every cell at a level has the same predicates and classes in its descriptions. */
  private lazy val sampleDescriptions: IndexedSeq[Vector[Seq[Quad]]] =
    Levels.map(level => descriptions.map(description => description(S2CellId.begin(level))))

  /** Deepest level at which cells can be found from their area, by scanning every cell at that level (up to about 100 ms) */
  val MaxAreaSearchLevel = 8

  /** Cells whose geo:hasMetricArea is `obj`. An area determines the level of its cells but not where they are: symmetry across the cube
    * gives up to 48 cells the same area. So cells are found by scanning their level, which is refused for levels deeper than
    * MaxAreaSearchLevel.
    */
  private def cellsWithArea(obj: Node, levels: Seq[Int]): Either[String, Seq[S2CellId]] =
    val value = Option
      .when(obj.isLiteral() && obj.getLiteralDatatypeURI() == XSDDatatype.XSDdouble.getURI())(obj.getLiteralLexicalForm())
      .flatMap(_.toDoubleOption)
      // area literals are only ever written in this form
      .filter(_.toString == obj.getLiteralLexicalForm())
    val level = value.flatMap(area => Levels.find(isAreaAtLevel(area, _))).filter(levels.contains)
    (value, level) match
      case (Some(area), Some(level)) if level > MaxAreaSearchLevel =>
        Left(
          s"Finding cells by geo:hasMetricArea is only supported for cells at levels 0 to $MaxAreaSearchLevel, but $area square meters is the area of a level $level cell."
        )
      case (Some(area), Some(level)) => Right(cellsAt(level).iterator.filter(cell => S2Graph.area(cell) == area).toSeq)
      case _                         => Right(Seq.empty)

  /** Whether an area in square meters is within the range of cell areas at a level. The ranges of different levels are far apart, so a
    * margin for the approximation used by `area` can't make them overlap.
    */
  private def isAreaAtLevel(area: Double, level: Int): Boolean =
    val steradians = area / (EarthRadiusMeters * EarthRadiusMeters)
    steradians >= S2Projections.PROJ.minArea.getValue(level) * 0.99 && steradians <= S2Projections.PROJ.maxArea.getValue(level) * 1.01

  private val CellLabel = """S2 Cell at level \d+ with ID (\d+)""".r
  private val GeometryLabel = """Geometry of the polygon formed from the vertices of the S2 Cell at level \d+ with ID (\d+)""".r
  private val WKTRing = """POLYGON \(\((.+)\)\)""".r

  /** Cells whose descriptions may contain an object other than a class. The object still needs to be checked against the description. */
  private def cellsNamedBy(obj: Node): Seq[S2CellId] =
    if obj.isURI() then toGeometryCell(obj).toSeq
    else if obj.isLiteral() then
      obj.getLiteralLexicalForm() match
        case CellLabel(id)     => parseCellID(id).toSeq
        case GeometryLabel(id) => parseCellID(id).toSeq
        case WKTRing(ring)     => cellsContainingRingCenter(ring)
        case id                => parseCellID(id).toSeq
    else Seq.empty

  /** The cells, one per level, containing the center of a ring of four cell vertices */
  private def cellsContainingRingCenter(ring: String): Seq[S2CellId] =
    val vertices = ring.split(", ").toSeq.map(_.split(" ").toSeq.flatMap(_.toDoubleOption))
    if vertices.size == 5 && vertices.forall(_.size == 2) then
      val center = vertices.take(4).map(vertex => S2LatLng.fromDegrees(vertex(1), vertex(0)).toPoint()).reduce(_.add(_)).normalize()
      val leaf = S2CellId.fromPoint(center)
      Levels.map(level => leaf.parent(level))
    else Seq.empty

  private def withinQuads(subject: Node, obj: Node, levels: Seq[Int]): SizedIterator[Quad] =
    (cellPattern(subject), cellPattern(obj)) match
      case (NoCell, _) | (_, NoCell) => SizedIterator.empty
      case (AnyCell, AnyCell) =>
        MultiSizedIterator(
          for
            face <- S2CellId.FACE_CELLS.toVector
            level <- levels
            if level > 0
          yield ContainmentIterator(face, level, false, 0, toWithinQuad)
        )
      case (OneCell(child), AnyCell) =>
        if levels.contains(child.level()) then
          SizedIterator.fromSeq(
            (0 until child.level()).map(parentLevel => toWithinQuad(ContainmentRelation(child.parent(parentLevel), child)))
          )
        else SizedIterator.empty
      case (AnyCell, OneCell(parent)) =>
        MultiSizedIterator(
          for level <- levels.toVector if level > parent.level()
          yield ContainmentIterator(parent, level, true, 0, toWithinQuad)
        )
      case (OneCell(child), OneCell(parent)) =>
        if levels.contains(child.level()) && parent.level() < child.level() && parent.contains(child) then
          SizedIterator.fromSeq(Seq(toWithinQuad(ContainmentRelation(parent, child))))
        else SizedIterator.empty

  private def containsQuads(subject: Node, obj: Node, levels: Seq[Int]): SizedIterator[Quad] =
    (cellPattern(subject), cellPattern(obj)) match
      case (NoCell, _) | (_, NoCell) => SizedIterator.empty
      case (AnyCell, AnyCell) =>
        MultiSizedIterator(
          for
            face <- S2CellId.FACE_CELLS.toVector
            level <- levels
            if level > 0
          yield ContainmentIterator(face, level, false, 0, toContainsQuad)
        )
      case (OneCell(parent), AnyCell) =>
        MultiSizedIterator(
          for level <- levels.toVector if level > parent.level()
          yield ContainmentIterator(parent, level, true, 0, toContainsQuad)
        )
      case (AnyCell, OneCell(child)) =>
        if levels.contains(child.level()) then
          SizedIterator.fromSeq(
            (0 until child.level()).map(parentLevel => toContainsQuad(ContainmentRelation(child.parent(parentLevel), child)))
          )
        else SizedIterator.empty
      case (OneCell(parent), OneCell(child)) =>
        if levels.contains(child.level()) && parent.level() < child.level() && parent.contains(child) then
          SizedIterator.fromSeq(Seq(toContainsQuad(ContainmentRelation(parent, child))))
        else SizedIterator.empty

  /** Like spatialkg, only cells at the same level are related by sfTouches */
  private def touchesQuads(subject: Node, obj: Node, levels: Seq[Int]): SizedIterator[Quad] =
    (cellPattern(subject), cellPattern(obj)) match
      case (NoCell, _) | (_, NoCell) => SizedIterator.empty
      case (AnyCell, AnyCell)        => MultiSizedIterator(levels.toVector.map(allTouches))
      case (OneCell(cell), AnyCell) =>
        if levels.contains(cell.level()) then SizedIterator.fromSeq(neighbors(cell).map(toTouchesQuad(cell, _)))
        else SizedIterator.empty
      case (AnyCell, OneCell(cell)) =>
        if levels.contains(cell.level()) then SizedIterator.fromSeq(neighbors(cell).map(toTouchesQuad(_, cell)))
        else SizedIterator.empty
      case (OneCell(cell), OneCell(other)) =>
        if levels.contains(cell.level()) && cell.level() == other.level() && neighbors(cell).contains(other) then
          SizedIterator.fromSeq(Seq(toTouchesQuad(cell, other)))
        else SizedIterator.empty

  /** sfTouches quads for every cell at a level. Only the cells in the corners of each cube face have fewer than eight neighbors, so the
    * runs of cells between corners can be skipped without enumerating them.
    */
  private def allTouches(level: Int): SizedIterator[Quad] =
    val begin = S2CellId.begin(level)
    val segments = Vector.newBuilder[SizedIterator[Quad]]
    var nextPosition = BigInt(0)
    def addRunUntil(end: BigInt): Unit =
      if end > nextPosition then
        val run = CellRangeIterator(begin.advance(nextPosition.toLong), end - nextPosition, identity[S2CellId])
        segments += FanOutIterator(run, 8, touchesFrom)
    for (position, touches) <- cornerTouches(level) do
      addRunUntil(position)
      segments += SizedIterator.fromSeq(touches)
      nextPosition = position + 1
    addRunUntil(BigInt(6) * BigInt(4).pow(level))
    MultiSizedIterator(segments.result())

  /** For each level, the sfTouches quads of the cells in the corners of the cube faces, by their position in `cellsAt` */
  private lazy val cornerTouches: IndexedSeq[Seq[(BigInt, Seq[Quad])]] =
    Levels.map { level =>
      val begin = S2CellId.begin(level)
      val corners =
        for
          face <- 0 until 6
          i <- Seq(0, S2CellId.MAX_SIZE - 1)
          j <- Seq(0, S2CellId.MAX_SIZE - 1)
        yield S2CellId.fromFaceIJ(face, i, j).parent(level)
      corners.distinct
        .map(corner =>
          BigInt(Long.divideUnsigned(corner.id() - begin.id(), 2 * S2CellId.lowestOnBitForLevel(level))) -> touchesFrom(corner)
        )
        .sortBy(_._1)
    }

  private def touchesFrom(cell: S2CellId): Seq[Quad] = neighbors(cell).map(toTouchesQuad(cell, _))

  /** spatial-full:connectedTo is a superproperty of sfWithin, sfContains, and sfTouches */
  private def connectedToQuads(subject: Node, obj: Node, levels: Seq[Int]): SizedIterator[Quad] =
    MultiSizedIterator(Vector(withinQuads(subject, obj, levels), containsQuads(subject, obj, levels), touchesQuads(subject, obj, levels)))
      .map(quad => Quad.create(quad.getGraph(), quad.getSubject(), ConnectedToNode, quad.getObject()))

  /** Cells sharing an edge or vertex with a cell, at the same level */
  def neighbors(cell: S2CellId): Seq[S2CellId] =
    val found = java.util.ArrayList[S2CellId]()
    cell.getAllNeighbors(cell.level(), found)
    // cells in the corners of a cube face are reported more than once
    found.asScala.toSeq.distinct.filterNot(_ == cell).sortWith((a, b) => a.compareTo(b) < 0)

  /** Every cell at a level, in Hilbert curve order across the six faces */
  def cellsAt(level: Int): SizedIterator[S2CellId] =
    CellRangeIterator(S2CellId.begin(level), BigInt(6) * BigInt(4).pow(level), identity[S2CellId])

  private def cellPattern(node: Node): CellPattern =
    if node.isVariable() then AnyCell
    else toS2Cell(node).map(OneCell(_)).getOrElse(NoCell)

  /** The cell named by an IRI. Only a cell's canonical IRI names it: the level must be the cell's level, and the ID must be written as a
    * plain unsigned integer.
    */
  def toS2Cell(node: Node): Option[S2CellId] = parseCellIRI(node, S2Prefix).filter(cell => s2CellIRI(cell) == node.getURI())

  /** The cell whose geometry is named by an IRI, which must be canonical */
  def toGeometryCell(node: Node): Option[S2CellId] = parseCellIRI(node, GeometryPrefix).filter(cell => geometryIRI(cell) == node.getURI())

  private def parseCellIRI(node: Node, prefix: String): Option[S2CellId] =
    if node.isURI() && node.getURI().startsWith(prefix) then
      node.getURI().stripPrefix(prefix).split("\\.", 2) match
        case Array(_, id) => parseCellID(id)
        case _            => None
    else None

  private def parseCellID(id: String): Option[S2CellId] = Try(Long.parseUnsignedLong(id)).toOption.map(S2CellId(_)).filter(_.isValid())

  def toS2Level(node: Node): Option[Int] =
    if node.isURI() && node.getURI().startsWith(S2Prefix) then
      node.getURI().stripPrefix(S2Prefix).toIntOption.filter(level => Graphs.get(level).contains(node))
    else None

  def toContainsQuad(relation: ContainmentRelation): Quad =
    val subj = NodeFactory.createURI(s2CellIRI(relation.parent))
    val obj = NodeFactory.createURI(s2CellIRI(relation.child))
    val graph = Graphs(relation.child.level())
    Quad.create(graph, subj, ContainsNode, obj)

  def toWithinQuad(relation: ContainmentRelation): Quad =
    val subj = NodeFactory.createURI(s2CellIRI(relation.child))
    val obj = NodeFactory.createURI(s2CellIRI(relation.parent))
    val graph = Graphs(relation.child.level())
    Quad.create(graph, subj, WithinNode, obj)

  def toTouchesQuad(cell: S2CellId, neighbor: S2CellId): Quad =
    Quad.create(Graphs(cell.level()), NodeFactory.createURI(s2CellIRI(cell)), TouchesNode, NodeFactory.createURI(s2CellIRI(neighbor)))

  /** `cellCount` consecutive cells, starting from `first`, at the level of `first` */
  final class CellRangeIterator[A](first: S2CellId, cellCount: BigInt, transform: S2CellId => A) extends SizedIterator[A]:
    override def iterator: Iterator[A] = new Iterator[S2CellId] {
      var nextCell = first
      var remaining = cellCount
      def hasNext: Boolean = remaining > 0
      def next(): S2CellId =
        val currentCell = nextCell
        nextCell = nextCell.next()
        remaining -= 1
        currentCell
    }.map(transform)

    override def size: BigInt = cellCount

    override def drop(num: BigInt): SizedIterator[A] =
      if num >= cellCount then SizedIterator.empty
      else CellRangeIterator(first.advance(num.toLong), cellCount - num, transform)

    override def map[B](f: A => B): SizedIterator[B] = CellRangeIterator(first, cellCount, transform.andThen(f))

  /** Level of container cell must be greater than level
    * @param containerCell
    *   must not be level 30
    * @param level
    *   level of contained cells to iterate
    * @param direct
    *   if false produce containment relations including all intervening parent cells
    * @param offset
    */
  class ContainmentIterator[A](
      containerCell: S2CellId,
      level: Int,
      direct: Boolean = false,
      offset: BigInt = 0,
      transform: ContainmentRelation => A = identity
  ) extends SizedIterator[A]:
    val levelDepth = level - containerCell.level()
    val multiplier = if (direct) 1 else levelDepth
    val (advance, remainder) = offset /% multiplier
    val start = containerCell.childBegin(level)
    val end = containerCell.childEnd(level)

    override def iterator: Iterator[A] = new Iterator[ContainmentRelation] {
      val containerLevels = containerCell.level until level
      var nextCell = start.advance(advance.toLong)
      var containers = containerLevels.map(nextCell.parent(_)).toList.drop(remainder.toInt)
      def hasNext: Boolean = nextCell != end
      def next(): ContainmentRelation =
        val currentCell = nextCell
        if direct then
          nextCell = nextCell.next()
          ContainmentRelation(containerCell, currentCell)
        else
          val (container :: newContainers) = containers: @unchecked
          containers = newContainers
          if containers.isEmpty then
            nextCell = nextCell.next()
            containers = containerLevels.map(nextCell.parent(_)).toList
          ContainmentRelation(container, currentCell)
    }.map(transform)

    override def size: BigInt = (BigInt(4).pow(levelDepth) * multiplier) - offset

    override def drop(num: BigInt): SizedIterator[A] =
      ContainmentIterator(containerCell, level, direct, offset + num, transform)

    override def map[B](f: A => B): SizedIterator[B] =
      ContainmentIterator(containerCell, level, direct, offset, transform.andThen(f))
