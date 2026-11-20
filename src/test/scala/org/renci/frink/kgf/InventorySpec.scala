package org.renci.frink.kgf

import com.google.common.geometry.S2CellId
import org.renci.frink.s2.S2Graph
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The closed-form namespace inventory, checked against the cells themselves at the levels small enough to enumerate */
class InventorySpec extends AnyFlatSpec with Matchers:
  import Inventory.Role

  private val resource = S2Graph.KWGResource
  private val cellStem = (level: Int) => s"${S2Graph.S2Prefix}$level."
  private val geometryStem = (level: Int) => s"${S2Graph.GeometryPrefix}$level."

  private def ids(prefix: String, role: Role = Role.Subject): Seq[String] =
    Inventory.terms(prefix, role).iterator.map(_.getURI()).toSeq

  "Inventory" should "count every cell at a level" in {
    for level <- 0 to 6 do
      Inventory.count(cellStem(level), Role.Subject) shouldBe Model.cells(level)
      Inventory.count(geometryStem(level), Role.Subject) shouldBe Model.cells(level)
  }

  it should "enumerate exactly the ids the S2 library gives, in ascending order" in {
    for level <- 0 to 4 do
      val expected = S2Graph.cellsAt(level).iterator.map(cell => java.lang.Long.toUnsignedString(cell.id())).toSeq
      val found = ids(cellStem(level)).map(_.stripPrefix(cellStem(level)))
      found should contain theSameElementsAs expected
      found.map(BigInt(_)) shouldBe found.map(BigInt(_)).sorted
  }

  it should "count the cells under a decimal prefix of an id" in {
    for
      level <- 0 to 4
      digits <- Seq("1", "2", "3", "9", "11", "13", "4611686018427387904")
    do
      val all = S2Graph.cellsAt(level).iterator.map(cell => java.lang.Long.toUnsignedString(cell.id())).toSeq
      val expected = all.count(_.startsWith(digits))
      withClue(s"level $level, ids starting with $digits: ") {
        Inventory.count(s"${cellStem(level)}$digits", Role.Subject) shouldBe BigInt(expected)
      }
  }

  it should "count nothing under a prefix no id can have" in {
    Inventory.count(s"${cellStem(13)}0", Role.Subject) shouldBe BigInt(0)
    Inventory.count(s"${cellStem(13)}x", Role.Subject) shouldBe BigInt(0)
    Inventory.count(s"${cellStem(31)}", Role.Subject) shouldBe BigInt(0)
    Inventory.count("http://example.org/", Role.Any) shouldBe BigInt(0)
  }

  it should "count every cell of every level under the cell prefix, and the geometries under theirs" in {
    val cells = Model.Levels.map(Model.cells).sum
    Inventory.count(s"${resource}s2.", Role.Subject) shouldBe cells
    Inventory.count(s"${resource}geometry.polygon.s2.", Role.Subject) shouldBe cells
    Inventory.count(resource, Role.Subject) shouldBe 2 * cells
    // 9,223,372,036,854,775,806 cells: two for every value a signed 64-bit integer can hold, less two
    cells shouldBe BigInt("9223372036854775806")
  }

  it should "separate the roles, and read `any` as their union rather than their sum" in {
    val ontology = S2Graph.KWGOnt
    Inventory.count(s"${ontology}sf", Role.Predicate) shouldBe BigInt(3)
    Inventory.count(s"${ontology}sf", Role.Object) shouldBe BigInt(0)
    Inventory.count(s"${ontology}sf", Role.Any) shouldBe BigInt(3)
    Inventory.count(s"${ontology}S2Cell", Role.Object) shouldBe BigInt(32)
    Inventory.count(s"${ontology}S2Cell", Role.Predicate) shouldBe BigInt(0)
    val cells = Model.Levels.map(Model.cells).sum
    // a cell IRI is a subject and an object, and one term
    Inventory.count(s"${resource}s2.", Role.Any) shouldBe cells
    Inventory.count(s"${resource}s2.", Role.Object) shouldBe cells
  }

  it should "count a level graph IRI as no term, since a graph name fills no triple position" in {
    // the prefix of every level 13 cell IRI, and the name of that level's graph, which `/graphs` lists instead
    Inventory.count(s"${S2Graph.S2Prefix}13", Role.Any) shouldBe Model.cells(13)
    val first = java.lang.Long.toUnsignedString(S2CellId.begin(13).id())
    Inventory.terms(s"${S2Graph.S2Prefix}13", Role.Any).iterator.next().getURI() shouldBe s"${cellStem(13)}$first"
  }

  it should "skip positions without enumerating them, so a deep page costs what the first costs" in {
    val terms = Inventory.terms(cellStem(13), Role.Subject)
    for skip <- Seq(BigInt(0), BigInt(1), BigInt(7), BigInt(1000)) do
      withClue(s"dropping $skip: ") {
        terms.drop(skip).iterator.take(3).toSeq shouldBe terms.iterator.drop(skip.toInt).take(3).toSeq
      }
    val deep = BigInt("100000000")
    terms.drop(deep).size shouldBe terms.size - deep
    val expected = java.lang.Long.toUnsignedString(S2CellId.begin(13).advance(deep.toLong).id())
    terms.drop(deep).iterator.next().getURI() shouldBe s"${cellStem(13)}$expected"
  }

  it should "report which roles a term fills" in {
    Inventory.rolesOf(S2Graph.ContainsNode) shouldBe Seq("predicate")
    Inventory.rolesOf(S2Graph.S2CellClass) shouldBe Seq("object")
    Inventory.rolesOf(S2Graph.Graphs(13)) shouldBe Seq.empty
  }
