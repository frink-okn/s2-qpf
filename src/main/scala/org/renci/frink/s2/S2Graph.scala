package org.renci.frink.s2

import com.google.common.geometry.S2CellId
import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.apache.jena.sparql.core.Quad
import org.renci.frink.Util.MultiSizedIterator
import org.renci.frink.Util.SingleSizedIterator
import org.renci.frink.Util.SizedIterator

import java.lang.Long
import scala.util.Try

object S2Graph:
  val S2Prefix = "http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level"
  val KWGOnt = "http://stko-kwg.geog.ucsb.edu/lod/ontology/"
  val Within = s"${KWGOnt}sfWithin"
  val Contains = s"${KWGOnt}sfContains"
  val ContainsNode = NodeFactory.createURI(Contains)
  val WithinNode = NodeFactory.createURI(Within)
  val Graphs = (0 to 30).map(level => level -> NodeFactory.createURI(s"$S2Prefix$level")).toMap

  final case class ContainmentRelation(parent: S2CellId, child: S2CellId)

  def s2CellIRI(cell: S2CellId): String = s"$S2Prefix${cell.level()}.${Long.toUnsignedString(cell.id())}"

  def quads(pattern: Quad, offset: Int = 0): SizedIterator[Quad] =
    val rels = toS2Relations(pattern.getPredicate())
    val subj = toS2Cell(pattern.getSubject())
    val obj = toS2Cell(pattern.getObject())
    if rels.isEmpty then SizedIterator.empty
    else if pattern.getSubject().matches(pattern.getObject()) then SizedIterator.empty
    else if !pattern.getSubject().isVariable() && subj.isEmpty then SizedIterator.empty
    else if !pattern.getObject().isVariable() && obj.isEmpty then SizedIterator.empty
    else
      val lev = toS2Level(pattern.getGraph())
      val levels = lev.map(Seq(_)).getOrElse(1 to 30)
      val mainIterator = (subj, obj) match

        case (None, None) =>
          val iterators =
            for
              face <- S2CellId.FACE_CELLS
              level <- levels
            yield
              val containsQuads =
                if rels(Contains) then ContainmentIterator(face, level, false, 0, toContainsQuad)
                else SizedIterator.empty
              val withinQuads =
                if rels(Within) then ContainmentIterator(face, level, false, 0, toWithinQuad)
                else SizedIterator.empty
              MultiSizedIterator(Vector(containsQuads, withinQuads))
          MultiSizedIterator(iterators.toVector)

        case (Some(subjectCell), None) =>
          val containsQuads =
            if rels(Contains) then
              for
                level <- levels
                if level > subjectCell.level()
              yield ContainmentIterator(subjectCell, level, true, offset, toContainsQuad)
            else Vector.empty
          val withinQuads =
            if rels(Within) && levels.contains(subjectCell.level()) then
              for parentLevel <- 0 until subjectCell.level()
              yield toWithinQuad(ContainmentRelation(subjectCell.parent(parentLevel), subjectCell))
            else Seq.empty
          MultiSizedIterator(containsQuads.toVector.prepended(new SingleSizedIterator(withinQuads.iterator, withinQuads.size)))

        case (None, Some(objectCell)) =>
          val withinQuads =
            if rels(Within) then
              for
                level <- levels
                if level > objectCell.level()
              yield ContainmentIterator(objectCell, level, true, offset, toWithinQuad)
            else Vector.empty
          val containsQuads =
            if rels(Contains) && levels.contains(objectCell.level()) then
              for parentLevel <- 0 until objectCell.level()
              yield toContainsQuad(ContainmentRelation(objectCell.parent(parentLevel), objectCell))
            else Seq.empty
          MultiSizedIterator(withinQuads.toVector.prepended(new SingleSizedIterator(containsQuads.iterator, containsQuads.size)))

        case (Some(subjectCell), Some(objectCell)) =>
          val quad =
            if rels(Contains) && levels.contains(objectCell.level()) && subjectCell.contains(objectCell) then
              Some(toContainsQuad(ContainmentRelation(subjectCell, objectCell)))
            else if rels(Within) && levels.contains(subjectCell.level()) && objectCell.contains(subjectCell) then
              Some(toWithinQuad(ContainmentRelation(objectCell, subjectCell)))
            else None
          SingleSizedIterator(quad.iterator, quad.size)

      mainIterator.drop(offset)

  def toS2Cell(node: Node): Option[S2CellId] =
    if node.isURI() then
      val iri = node.getURI()
      if iri.startsWith(S2Prefix) then
        val parts = iri.replace(S2Prefix, "").split("\\.", 2)
        if parts.size == 2 then
          for
            level <- parts(0).toIntOption
            cellID <- Try(Long.parseUnsignedLong(parts(1))).toOption
          yield S2CellId(cellID)
        else None
      else None
    else None

  def toS2Level(node: Node): Option[Int] =
    if node.isURI() then
      val iri = node.getURI()
      if iri.startsWith(S2Prefix) then
        node
          .getURI()
          .replace(S2Prefix, "")
          .toIntOption
          .filterNot(_ < 0)
          .filterNot(_ > 30)
      else None
    else None

  def toS2Relations(node: Node): Set[String] =
    if node.isVariable() then Set(Within, Contains)
    else if node.isURI() then
      node.getURI() match
        case Within   => Set(Within)
        case Contains => Set(Contains)
        case _        => Set.empty
    else Set.empty

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
