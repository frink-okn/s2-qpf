package org.renci.frink.kgf

import org.apache.jena.graph.Node
import org.apache.jena.sparql.core.Quad
import org.renci.frink.qpf.QuadPatternFragment.UnionGraph
import org.renci.frink.s2.S2Graph
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The description model against the patterns it claims to describe.
  *
  * Doc 04's invariant is that the top-level statistics equal `/count`. Here that is checkable rather than asserted: every statistic is a
  * closed form, and every one of them has a pattern whose size must equal it.
  */
class ModelSpec extends AnyFlatSpec with Matchers:

  private val sampleLevels = Seq(0, 1, 2, 5, 13, 30)

  private def size(graph: Node, predicate: Node): BigInt =
    S2Graph.quads(Quad.create(graph, Term.freshVariable, predicate, Term.freshVariable)).fold(fail(_), _.size)

  "The model" should "count the whole dataset as the server counts it" in {
    val total = size(UnionGraph, Term.freshVariable)
    Model.Queryable.triples shouldBe total
    // the figure the OKN registry publishes for this graph
    total shouldBe BigInt("1361984604108888559446")
  }

  it should "decompose a level graph into property partitions that sum to its quads" in {
    for level <- sampleLevels do
      val view = Model.levelView(level)
      withClue(s"level $level: ") {
        view.properties.map(_.triples).sum shouldBe size(S2Graph.Graphs(level), Term.freshVariable)
      }
  }

  it should "state each predicate's triples as that predicate's pattern counts them, per level" in {
    for
      level <- sampleLevels
      predicate <- Model.Predicates
    do
      val stated = Model.levelView(level).property(predicate).map(_.triples).getOrElse(BigInt(0))
      withClue(s"level $level, $predicate: ") {
        stated shouldBe size(S2Graph.Graphs(level), predicate)
      }
  }

  it should "state each predicate's triples over the whole dataset as the union pattern counts them" in {
    for predicate <- Model.Predicates do
      val stated = Model.Queryable.property(predicate).map(_.triples).getOrElse(BigInt(0))
      withClue(s"$predicate: ") {
        stated shouldBe size(UnionGraph, predicate)
      }
  }

  it should "put every class partition's triples under geo:SpatialObject, which holds every subject" in {
    for level <- sampleLevels do
      val view = Model.levelView(level)
      val spatialObject = view.classPartition(S2Graph.SpatialObjectClass).getOrElse(fail("no geo:SpatialObject partition"))
      withClue(s"level $level: ") {
        spatialObject.triples shouldBe view.triples
        spatialObject.entities shouldBe view.subjects
      }
  }

  it should "count a cell and its geometry as subjects, and nothing else" in {
    val cells = Model.Levels.map(Model.cells).sum
    Model.Queryable.subjects shouldBe 2 * cells
    Model.Queryable.classPartition(S2Graph.GeometryClass).map(_.entities) shouldBe Some(cells)
    Model.Queryable.classPartition(S2Graph.S2CellClass).map(_.entities) shouldBe Some(cells)
  }

  it should "describe a level graph's ancestors, which its containment quads have as subjects" in {
    val view = Model.levelView(3)
    // a containment quad lives in the child's graph, so level 3's graph has every cell above level 3 as a subject
    view.cellCount shouldBe Model.cells(3) + Model.ancestors(3)
    view.classPartition(S2Graph.LevelClasses(1)).map(_.entities) shouldBe Some(Model.cells(1))
    view.classPartition(S2Graph.LevelClasses(1)).map(_.triples) shouldBe Some(2 * Model.cells(3))
    view.classPartition(S2Graph.LevelClasses(4)) shouldBe None
  }

  it should "relate classes without pretending the rows partition the triples" in {
    val relations = Model.levelView(2).relations
    val touching = relations.filter(_.predicate == S2Graph.TouchesNode)
    // a cell is typed four ways at once, so one neighbour quad is reported under sixteen pairs of classes
    touching.size shouldBe 16
    touching.map(_.triples).distinct shouldBe Seq(Model.touches(2))
    relations
      .find(relation =>
        relation.subjectClass == S2Graph.LevelClasses(2) && relation.predicate == S2Graph.WithinNode &&
          relation.objectClass == S2Graph.LevelClasses(0)
      )
      .map(_.triples) shouldBe Some(Model.cells(2))
    relations.exists(_.predicate == org.apache.jena.vocabulary.RDF.Nodes.`type`) shouldBe false
  }

  it should "offer a view per level graph, and the design view as the dataset itself" in {
    Model.view("queryable").map(_.triples) shouldBe Some(Model.Queryable.triples)
    Model.view("design").map(_.triples) shouldBe Some(Model.Queryable.triples)
    Model.view(s"graph:${S2Graph.Graphs(13).getURI()}").map(_.levels) shouldBe Some(Set(13))
    Model.view("graph:http://example.org/") shouldBe None
    Model.ViewNames.size shouldBe 33
  }
