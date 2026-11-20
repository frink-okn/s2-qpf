package org.renci.frink.kgf

import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.renci.frink.Util.MultiSizedIterator
import org.renci.frink.Util.SizedIterator
import org.renci.frink.s2.S2Graph

/** The IRIs this server holds, counted and enumerated under a prefix in closed form: `/terms` (KGF doc 03 section 3.4.8).
  *
  * This is the only evidence a computed graph can give a federation index, which leaves a graph out of an answer only on evidence that the
  * graph cannot match (KGF doc 05 section 5.8). It must be complete — every IRI this server emits lies under one of the shared table's
  * prefixes — and countable for any prefix, including one nested inside the cell prefixes, such as a prefix for a single level.
  *
  * How the closed form works. The valid cell ids at level L are an arithmetic progression: first 2^(60-2L), step 2^(61-2L), 6*4^L of them.
  * A decimal prefix R selects the ids in the ranges [R*10^k, R*10^k + 10^k - 1], so a count is one division per range and a page is a
  * position in a concatenation of sub-progressions — exact, and independent of how many ids match.
  */
object Inventory:

  enum Role(val name: String):
    case Subject extends Role("subject")
    case Predicate extends Role("predicate")
    case Object extends Role("object")
    case Any extends Role("any")

  object Role:
    def parse(name: String): Either[Problem, Role] =
      Role.values
        .find(_.name == name)
        .map(Right(_))
        .getOrElse(Left(Problem.malformed(s"role=$name is not a role; it is one of ${Role.values.map(_.name).mkString(", ")}")))

  /** A cell IRI and a geometry IRI each fill a subject and an object position; the eleven predicates fill a predicate position; the classes
    * are only ever objects. A level graph IRI is a graph name and fills no triple position, so it is not a term here — `/graphs` lists
    * those.
    */
  private val CellRoles = Set(Role.Subject, Role.Object, Role.Any)
  private val PredicateRoles = Set(Role.Predicate, Role.Any)
  private val ObjectRoles = Set(Role.Object, Role.Any)

  private sealed trait Family:
    def stem: String
    def roles: Set[Role]

  /** One IRI from the fixed vocabulary */
  private final case class One(stem: String, roles: Set[Role]) extends Family

  /** Every cell IRI, or every geometry IRI, at one level */
  private final case class Cells(stem: String, level: Int, roles: Set[Role] = CellRoles) extends Family

  private val families: Seq[Family] =
    val fixed = Model.Predicates.map(predicate => One(predicate.getURI(), PredicateRoles)) ++
      Model.Classes.map(classTerm => One(classTerm.getURI(), ObjectRoles))
    val cells = Model.Levels.map(level => Cells(s"${S2Graph.S2Prefix}$level.", level))
    val geometries = Model.Levels.map(level => Cells(s"${S2Graph.GeometryPrefix}$level.", level))
    // Stems are pairwise non-prefixing, so each family is one contiguous block of the order and whole families can be skipped by size
    (fixed ++ cells ++ geometries).sortBy(_.stem)

  /** The number of distinct IRIs under a prefix in a role. `any` is the union of the three roles, not their sum: a cell IRI is a subject
    * and an object and is one term.
    */
  def count(prefix: String, role: Role): BigInt = terms(prefix, role).size

  def counts(prefix: String): Seq[(Role, BigInt)] = Role.values.toVector.map(role => role -> count(prefix, role))

  /** The IRIs under a prefix, in a fixed order: families by IRI stem, then cell ids ascending.
    *
    * Ascending by id rather than by the lexicographic spelling of the id. Doc 03 describes a scan over a sorted dictionary, and this server
    * has no dictionary to sort: the set of terms is the same either way, numeric order is what a cell id means, and it is what makes `drop`
    * an arithmetic skip rather than a walk of a decimal trie. `docs/kgf.md` section 10 records that.
    */
  def terms(prefix: String, role: Role): SizedIterator[Node] =
    MultiSizedIterator(families.toVector.filter(_.roles.contains(role)).map(members(_, prefix)))

  /** Which roles a term fills, for the `roles` array of a `/terms` row */
  def rolesOf(term: Node): Seq[String] =
    val iri = term.getURI()
    families
      .find(family => iri.startsWith(family.stem) || iri == family.stem)
      .toVector
      .flatMap(_.roles.filterNot(_ == Role.Any).toVector.sortBy(_.name).map(_.name))

  private def members(family: Family, prefix: String): SizedIterator[Node] =
    family match
      case One(iri, _) =>
        if iri.startsWith(prefix) then SizedIterator.fromSeq(Vector(NodeFactory.createURI(iri))) else SizedIterator.empty
      case Cells(stem, level, _) =>
        if prefix.length <= stem.length then
          if stem.startsWith(prefix) then ids(level, begin(level), maxId(level)).map(iri(stem, _)) else SizedIterator.empty
        else if prefix.startsWith(stem) then withDecimalPrefix(level, prefix.drop(stem.length)).map(iri(stem, _))
        else SizedIterator.empty

  private def iri(stem: String, id: BigInt): Node = NodeFactory.createURI(s"$stem$id")

  /** Cell ids at a level whose decimal spelling starts with `digits`. No id is written with a leading zero, so a prefix that starts with
    * one matches nothing, as does one that is not all digits.
    */
  private def withDecimalPrefix(level: Int, digits: String): SizedIterator[BigInt] =
    if digits.isEmpty || !digits.forall(_.isDigit) || digits.startsWith("0") then SizedIterator.empty
    else
      val value = BigInt(digits)
      // An id has at most twenty digits, so the ranges below are the only ones that can hold one
      val ranges = (0 to (20 - digits.length).max(0)).toVector.map { extra =>
        val factor = BigInt(10).pow(extra)
        (value * factor, value * factor + factor - 1)
      }
      MultiSizedIterator(ranges.map((low, high) => ids(level, low, high)))

  /** Valid cell ids at a level within an inclusive range, ascending */
  private def ids(level: Int, low: BigInt, high: BigInt): SizedIterator[BigInt] =
    val stride = step(level)
    val first = begin(level)
    val lowest = low.max(first)
    val highest = high.min(maxId(level))
    if lowest > highest then SizedIterator.empty
    else
      val fromIndex = (lowest - first + stride - 1) / stride
      val toIndex = (highest - first) / stride
      if toIndex < fromIndex then SizedIterator.empty
      else Progression(first + fromIndex * stride, stride, toIndex - fromIndex + 1)

  /** The id of the first cell at a level: face 0, position 0, with the level's lowest bit set */
  private def begin(level: Int): BigInt = BigInt(2).pow(60 - 2 * level)

  /** Consecutive cells at a level differ by twice that lowest bit */
  private def step(level: Int): BigInt = BigInt(2).pow(61 - 2 * level)

  private def maxId(level: Int): BigInt = begin(level) + (Model.cells(level) - 1) * step(level)

  /** `count` terms of an arithmetic progression, skippable arithmetically */
  private final class Progression(first: BigInt, stride: BigInt, total: BigInt) extends SizedIterator[BigInt]:
    def iterator: Iterator[BigInt] =
      val limit = total
      new Iterator[BigInt] {
        private var value = first
        private var left = limit
        def hasNext: Boolean = left > 0
        def next(): BigInt =
          val current = value
          value += stride
          left -= 1
          current
      }

    def size: BigInt = total

    def drop(num: BigInt): SizedIterator[BigInt] =
      if num >= total then SizedIterator.empty else Progression(first + num * stride, stride, total - num)

    def map[B](f: BigInt => B): SizedIterator[B] = MappedProgression(this, f)

  /** A progression under a transformation, which keeps `drop` arithmetic */
  private final class MappedProgression[A, B](source: SizedIterator[A], transform: A => B) extends SizedIterator[B]:
    def iterator: Iterator[B] = source.iterator.map(transform)

    def size: BigInt = source.size

    def drop(num: BigInt): SizedIterator[B] = MappedProgression(source.drop(num), transform)

    def map[C](f: B => C): SizedIterator[C] = MappedProgression(source, transform.andThen(f))
