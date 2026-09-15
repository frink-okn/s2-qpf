package org.renci.frink.s2

import com.google.common.geometry.S2Cell
import com.google.common.geometry.S2CellId
import com.google.common.geometry.S2LatLng
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.apache.jena.sparql.core.Quad
import org.apache.jena.vocabulary.RDF.Nodes as RDF
import org.apache.jena.vocabulary.RDFS.Nodes as RDFS
import org.renci.frink.qpf.Bindings
import org.renci.frink.qpf.QuadPatternFragment.UnionGraph
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class S2GraphSpec extends AnyFlatSpec with Matchers:
  import S2Graph.*

  private def iri(value: String) = NodeFactory.createURI(value)
  private def variable(name: String) = NodeFactory.createVariable(name)
  private def cellIRI(level: Int, id: String) = iri(s"${S2Prefix}$level.$id")
  private def pattern(s: Node = variable("s"), p: Node = variable("p"), o: Node = variable("o"), g: Node = variable("g")) =
    Quad.create(g, s, p, o)
  private def objects(quads: Seq[Quad], predicate: Node) = quads.filter(_.getPredicate() == predicate).map(_.getObject()).toSet
  private def matching(queried: Quad) = S2Graph.quads(queried).fold(reason => fail(reason), identity)
  private def restricted(queried: Quad, values: String) =
    S2Graph.quads(queried, Bindings.parse(values).get).fold(reason => fail(reason), identity)

  /** Longitude/latitude rings of a POLYGON or MULTIPOLYGON */
  private def rings(wkt: String): Seq[Seq[(Double, Double)]] =
    """\(([-\d. ,]+)\)""".r
      .findAllMatchIn(wkt)
      .map(_.group(1).split(", ").toSeq.map(_.split(" ") match { case Array(lng, lat) => (lng.toDouble, lat.toDouble) }))
      .toSeq

  private def contains(ring: Seq[(Double, Double)], x: Double, y: Double): Boolean =
    ring.zip(ring.tail).count { case ((x1, y1), (x2, y2)) => (y1 > y) != (y2 > y) && x < x1 + (y - y1) * (x2 - x1) / (y2 - y1) } % 2 == 1

  // A York County, Maine cell from SAWGraph spatialkg v0.0.6
  private val yorkCell = cellIRI(13, "5525166171478818816")
  private val yorkGeometry = iri(s"${GeometryPrefix}13.5525166171478818816")

  "S2Graph" should "describe a level 13 cell the way SAWGraph spatialkg does" in {
    // sfContains is left out, since it relates a cell to every one of its descendants
    val quads = matching(pattern(s = yorkCell)).iterator.takeWhile(_.getPredicate() != ContainsNode).toSeq ++
      matching(pattern(s = yorkCell, p = TouchesNode)).iterator.toSeq
    objects(quads, RDF.`type`) shouldBe Set(S2CellClass, LevelClasses(13), FeatureClass, SpatialObjectClass)
    objects(quads, RDFS.label) shouldBe Set(NodeFactory.createLiteralString("S2 Cell at level 13 with ID 5525166171478818816"))
    objects(quads, CellIDNode) shouldBe Set(NodeFactory.createLiteralDT("5525166171478818816", XSDDatatype.XSDinteger))
    objects(quads, HasGeometryNode) shouldBe Set(yorkGeometry)
    objects(quads, DefaultGeometryNode) shouldBe Set(yorkGeometry)
    objects(quads, HasMetricAreaNode) shouldBe Set(NodeFactory.createLiteralDT("999757.1006920862", XSDDatatype.XSDdouble))
    objects(quads, WithinNode) should contain(cellIRI(12, "5525166119939211264"))
    objects(quads, TouchesNode) shouldBe Set(
      "5525166068399603712",
      "5525166102759342080",
      "5525166137119080448",
      "5525166205838557184",
      "5525166240198295552",
      "5526667279728640000",
      "5526667314088378368",
      "5526667348448116736"
    ).map(cellIRI(13, _))
    quads.map(_.getGraph()).toSet shouldBe Set(Graphs(13))
  }

  it should "describe a cell's geometry the way SAWGraph spatialkg does" in {
    val quads = matching(pattern(s = yorkGeometry)).iterator.toSeq
    objects(quads, RDF.`type`) shouldBe Set(GeometryClass, SpatialObjectClass)
    objects(quads, RDFS.label) shouldBe Set(
      NodeFactory.createLiteralString(
        "Geometry of the polygon formed from the vertices of the S2 Cell at level 13 with ID 5525166171478818816"
      )
    )
    objects(quads, AsWKTNode) shouldBe Set(
      NodeFactory.createLiteralDT(
        "POLYGON ((-70.40760448040841 43.311023338366375, -70.41951215334895 43.31313814067609, -70.42540226378415 43.30487161725251, -70.41349759662894 43.30275807316771, -70.40760448040841 43.311023338366375))",
        WKTLiteral
      )
    )
  }

  it should "only accept canonical cell IRIs" in {
    toS2Cell(yorkCell).map(_.level()) shouldBe Some(13)
    toS2Cell(cellIRI(5, "5525166171478818816")) shouldBe None
    toS2Cell(cellIRI(13, "05525166171478818816")) shouldBe None
    toS2Cell(cellIRI(13, "12345")) shouldBe None
    toS2Cell(cellIRI(30, "12345")).map(_.level()) shouldBe Some(30)
    toS2Cell(cellIRI(30, "2")) shouldBe None
    toGeometryCell(iri(s"${GeometryPrefix}5.5525166171478818816")) shouldBe None
    toS2Level(iri(s"${S2Prefix}013")) shouldBe None
    matching(pattern(s = cellIRI(5, "5525166171478818816"))).size shouldBe 0
    matching(pattern(o = cellIRI(30, "2"))).size shouldBe 0
  }

  it should "find subjects from literal objects" in {
    val yorkDescription =
      matching(pattern(s = yorkCell)).iterator.take(9).toSeq ++ matching(pattern(s = yorkGeometry)).iterator.toSeq
    val literalQuads = yorkDescription.filter(quad => quad.getObject().isLiteral() && quad.getPredicate() != HasMetricAreaNode)
    literalQuads.map(_.getPredicate()).toSet shouldBe Set(RDFS.label, CellIDNode, AsWKTNode)
    for quad <- literalQuads do matching(pattern(p = quad.getPredicate(), o = quad.getObject())).iterator.toSeq shouldBe Seq(quad)
    matching(pattern(o = NodeFactory.createLiteralDT("05525166171478818816", XSDDatatype.XSDinteger))).size shouldBe 0
    matching(pattern(o = NodeFactory.createLiteralString("S2 Cell at level 12 with ID 5525166171478818816"))).size shouldBe 0
    matching(pattern(s = variable("s"), o = yorkGeometry)).iterator.map(_.getSubject()).toSet shouldBe Set(yorkCell)
  }

  it should "write valid geometry for every cell, including at the poles and the antimeridian" in {
    val random = scala.util.Random(13)
    val edgeCases =
      for
        level <- Seq(1, 13, 30)
        (lat, lng) <- Seq(90.0 -> 45.0, -90.0 -> -45.0, 10.0 -> 179.99999, 10.0 -> -179.99999, 89.99999 -> 180.0, -89.99999 -> -179.99999)
      yield S2CellId.fromLatLng(S2LatLng.fromDegrees(lat, lng)).parent(level)
    val randomCells = (1 to 500).map(_ =>
      S2CellId.fromLatLng(S2LatLng.fromDegrees(random.between(-90.0, 90.0), random.between(-180.0, 180.0))).parent(random.between(4, 31))
    )
    val cells = (0 to 3).flatMap(cellsAt(_).iterator) ++ edgeCases ++ randomCells
    for cell <- cells do
      val literal = wkt(cell)
      withClue(s"level ${cell.level()} $literal: ") {
        val polygons = rings(literal)
        polygons should not be empty
        for ring <- polygons do
          ring.head shouldBe ring.last
          ring.forall((lng, lat) => lng >= -180 && lng <= 180 && lat >= -90 && lat <= 90) shouldBe true
          // counterclockwise, with some area, measured from the first point to keep precision for tiny cells
          val (x0, y0) = ring.head
          ring.zip(ring.tail).map { case ((x1, y1), (x2, y2)) => (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0) }.sum should be > 0.0
        val inside = S2LatLng(S2Cell(if cell.level() < 30 then cell.child(0) else cell).getCenter())
        polygons.exists(ring => contains(ring, inside.lngDegrees(), inside.latDegrees())) shouldBe true
        val lookup = matching(pattern(p = AsWKTNode, o = NodeFactory.createLiteralDT(literal, WKTLiteral)))
        lookup.iterator.map(_.getSubject()).toSeq shouldBe Seq(iri(geometryIRI(cell)))
      }
    wkt(S2CellId.fromFace(2)) should include("180.0 90.0, -180.0 90.0")
    wkt(S2CellId.fromFace(3)) should startWith("MULTIPOLYGON")
  }

  it should "find cells from their area at coarse levels" in {
    val cell = S2CellId.begin(5).advance(1234)
    val areaQuad = matching(pattern(s = iri(s2CellIRI(cell)), p = HasMetricAreaNode)).iterator.toSeq.head
    val expected = cellsAt(5).iterator.filter(other => area(other) == area(cell)).map(s2CellIRI).map(iri).toSet
    expected.size should (be >= 8 and be <= 48)
    matching(pattern(p = HasMetricAreaNode, o = areaQuad.getObject())).iterator.map(_.getSubject()).toSet shouldBe expected
    matching(pattern(o = areaQuad.getObject())).iterator.map(_.getSubject()).toSet shouldBe expected
    matching(pattern(p = HasMetricAreaNode, o = areaQuad.getObject(), g = Graphs(6))).size shouldBe 0
    matching(pattern(p = RDFS.label, o = areaQuad.getObject())).size shouldBe 0
  }

  it should "refuse to find cells from their area at fine levels, unless no cell can match" in {
    val yorkArea = NodeFactory.createLiteralDT("999757.1006920862", XSDDatatype.XSDdouble)
    S2Graph.quads(pattern(p = HasMetricAreaNode, o = yorkArea)).left.map(_.contains("level 13")) shouldBe Left(true)
    S2Graph.quads(pattern(o = yorkArea)).isLeft shouldBe true
    // restricted to another level's graph
    matching(pattern(p = HasMetricAreaNode, o = yorkArea, g = Graphs(12))).size shouldBe 0
    // how spatialkg v0.0.6 writes the same area
    matching(pattern(p = HasMetricAreaNode, o = NodeFactory.createLiteralDT("999757.1006920862", XSDDatatype.XSDfloat))).size shouldBe 0
    // how the spatialkg SPARQL endpoint writes it: rounded, but still a double that some level 13 cell could have
    S2Graph
      .quads(pattern(p = HasMetricAreaNode, o = NodeFactory.createLiteralDT("999757.1006921", XSDDatatype.XSDdouble)))
      .isLeft shouldBe true
    // not written the way doubles are written here
    matching(pattern(p = HasMetricAreaNode, o = NodeFactory.createLiteralDT("999757.10069208620", XSDDatatype.XSDdouble))).size shouldBe 0
    // between the areas of level 17 and level 18 cells
    matching(pattern(p = HasMetricAreaNode, o = NodeFactory.createLiteralDT("2000.0", XSDDatatype.XSDdouble))).size shouldBe 0
    // with a subject, the area is just compared
    matching(pattern(s = yorkCell, o = yorkArea)).size shouldBe 1
  }

  it should "only return quads matching the requested pattern" in {
    val patterns = Seq(
      pattern(),
      pattern(s = yorkCell),
      pattern(o = yorkCell),
      pattern(s = yorkGeometry),
      pattern(o = yorkGeometry),
      pattern(p = TouchesNode),
      pattern(p = ConnectedToNode, o = yorkCell),
      pattern(p = RDF.`type`, o = LevelClasses(13)),
      pattern(p = RDF.`type`, o = SpatialObjectClass, g = Graphs(20)),
      pattern(o = GeometryClass),
      pattern(p = HasMetricAreaNode, g = Graphs(30)),
      pattern(s = yorkCell, p = WithinNode, o = cellIRI(12, "5525166119939211264")),
      pattern(s = yorkCell, g = Graphs(12)),
      pattern(s = yorkCell, p = TouchesNode, g = UnionGraph)
    )
    for
      queried <- patterns
      quad <- matching(queried).drop(12345).iterator.take(300) ++ matching(queried).iterator.take(300)
    do
      withClue(s"$queried returned $quad: ") {
        Seq(queried.getGraph() -> quad.getGraph(), queried.getSubject() -> quad.getSubject(), queried.getPredicate() -> quad.getPredicate())
          .appended(queried.getObject() -> quad.getObject())
          .forall((term, node) => term.isVariable() || term == node) shouldBe true
      }
  }

  it should "report sizes and pages consistently" in {
    val level1Cell = S2CellId.begin(1).next()
    val patterns = Seq(
      pattern(g = Graphs(0)),
      pattern(g = Graphs(2)),
      pattern(p = TouchesNode, g = Graphs(3)),
      pattern(p = ConnectedToNode, g = Graphs(2)),
      pattern(p = RDF.`type`, g = Graphs(3)),
      pattern(o = iri(s2CellIRI(level1Cell)), g = Graphs(3)),
      pattern(s = iri(s2CellIRI(level1Cell)), g = Graphs(3)),
      pattern(s = yorkCell, g = Graphs(13)),
      pattern(s = yorkCell, g = Graphs(15)),
      pattern(o = yorkCell, g = Graphs(15))
    )
    for queried <- patterns do
      withClue(s"$queried: ") {
        val all = matching(queried).iterator.toVector
        all should not be empty
        matching(queried).size shouldBe all.size
        all.distinct.size shouldBe all.size
        for offset <- Seq(1, 7, 99, 100, all.size / 3, all.size - 1, all.size) do
          matching(queried).drop(offset).iterator.toVector shouldBe all.drop(offset)
      }
  }

  it should "relate every cell at a level to its neighbors" in {
    for level <- 0 to 4 do
      val expected = cellsAt(level).iterator.flatMap(cell => neighbors(cell).map(toTouchesQuad(cell, _))).toSet
      val touches = matching(pattern(p = TouchesNode, g = Graphs(level)))
      touches.iterator.toSet shouldBe expected
      touches.size shouldBe expected.size
    neighbors(S2CellId.begin(0)).size shouldBe 4
    neighbors(S2CellId.begin(13)).size shouldBe 7
    neighbors(S2CellId.begin(13).next()).size shouldBe 8
  }

  it should "count every kind of pattern exactly" in {
    def cells(level: Int) = BigInt(6) * BigInt(4).pow(level)
    // cells in the corners of cube faces have 7 neighbors, and faces have 4
    def touches(level: Int) = if level == 0 then BigInt(24) else cells(level) * 8 - 24
    // sfWithin or sfContains triples in a level's graph, relating each cell to its ancestors
    def containment(level: Int) = cells(level) * level
    def descendants(level: Int) = (level + 1 to 30).map(deeper => BigInt(4).pow(deeper - level)).sum
    def allLevels(count: Int => BigInt) = Levels.map(count).sum
    def total(queried: Quad) = matching(queried).size
    // the first cell at a level is in a face corner
    def cornerCell(level: Int) = S2CellId.begin(level)
    def neighborCount(level: Int) = if level == 0 then 4 else 7

    total(pattern()) shouldBe allLevels(l => cells(l) * 13 + (containment(l) * 2 + touches(l)) * 2)
    for level <- Seq(0, 1, 13, 30) do
      total(pattern(g = Graphs(level))) shouldBe cells(level) * 13 + (containment(level) * 2 + touches(level)) * 2

    total(pattern(p = RDF.`type`)) shouldBe allLevels(cells(_) * 6)
    total(pattern(p = RDFS.label)) shouldBe allLevels(cells(_) * 2)
    for predicate <- Seq(CellIDNode, HasGeometryNode, DefaultGeometryNode, HasMetricAreaNode, AsWKTNode) do
      total(pattern(p = predicate)) shouldBe allLevels(cells)
    total(pattern(p = WithinNode)) shouldBe allLevels(containment)
    total(pattern(p = ContainsNode)) shouldBe allLevels(containment)
    total(pattern(p = TouchesNode)) shouldBe allLevels(touches)
    total(pattern(p = ConnectedToNode)) shouldBe allLevels(l => containment(l) * 2 + touches(l))

    total(pattern(o = S2CellClass)) shouldBe allLevels(cells)
    total(pattern(o = FeatureClass)) shouldBe allLevels(cells)
    total(pattern(o = GeometryClass)) shouldBe allLevels(cells)
    total(pattern(o = SpatialObjectClass)) shouldBe allLevels(cells(_) * 2)
    total(pattern(o = LevelClasses(13))) shouldBe cells(13)
    total(pattern(o = LevelClasses(13), g = Graphs(12))) shouldBe 0

    for level <- Seq(0, 13, 30) do
      val cell = iri(s2CellIRI(cornerCell(level)))
      val related = descendants(level) + level + neighborCount(level)
      // description, then sfWithin, sfContains, and sfTouches, then all of them again as connectedTo
      total(pattern(s = cell)) shouldBe 9 + related * 2
      total(pattern(o = cell)) shouldBe related * 2
      total(pattern(s = cell, g = Graphs(level))) shouldBe 9 + (level + neighborCount(level)) * 2
      if level < 30 then total(pattern(s = cell, g = Graphs(level + 2))) shouldBe 16 * 2
      if level > 0 then total(pattern(s = cell, g = Graphs(level - 1))) shouldBe 0
      val geometry = iri(geometryIRI(cornerCell(level)))
      total(pattern(s = geometry)) shouldBe 4
      total(pattern(o = geometry)) shouldBe 2

    // if a total were too high, the last page would come up short; if too low, there would be more quads after it
    for queried <- Seq(
        pattern(),
        pattern(p = TouchesNode),
        pattern(p = ConnectedToNode),
        pattern(p = RDF.`type`),
        pattern(o = iri(s2CellIRI(cornerCell(0))))
      )
    do
      val quads = matching(queried)
      quads.drop(quads.size - 100).iterator.take(101).size shouldBe 100
  }

  it should "count and page through the whole graph" in {
    val everything = matching(pattern())
    everything.size should be > BigInt(6) * BigInt(4).pow(30)
    val deep = everything.drop(BigInt(10).pow(18)).iterator.take(3).toSeq
    deep should have size 3
    val connected = matching(pattern(p = ConnectedToNode, g = Graphs(9))).size
    val parts = Seq(WithinNode, ContainsNode, TouchesNode).map(p => matching(pattern(p = p, g = Graphs(9))).size)
    connected shouldBe parts.sum
  }

  it should "match nothing when a variable is in more than one position" in {
    matching(pattern(s = variable("x"), o = variable("x"))).size shouldBe 0
    matching(pattern(s = variable("x"), g = variable("x"))).size shouldBe 0
    matching(pattern(p = variable("x"), o = variable("x"))).size shouldBe 0
  }

  it should "answer each quad compatible with several bindings once" in {
    val neighbor = cellIRI(13, "5525166068399603712")
    val queried = pattern(p = TouchesNode)
    val values =
      s"(?s ?o ?unrelated) { (<${yorkCell.getURI()}> UNDEF UNDEF) (UNDEF <${neighbor.getURI()}> 1) (<${yorkCell.getURI()}> UNDEF 2) }"
    val expected =
      (matching(pattern(s = yorkCell, p = TouchesNode)).iterator ++ matching(pattern(p = TouchesNode, o = neighbor)).iterator).toSet
    val matches = restricted(queried, values)
    matches.exact shouldBe false
    matches.positions.size shouldBe 16
    val answered = restricted(queried, values).positions.iterator.flatten.toVector
    answered.distinct shouldBe answered
    answered.toSet shouldBe expected
    answered.size shouldBe 15
    for offset <- 0 to 16 do
      restricted(queried, values).positions.drop(offset).iterator.toVector shouldBe
        restricted(queried, values).positions.iterator.toVector.drop(offset)
  }

  it should "count and page bindings exactly when they can't overlap" in {
    val parents = Seq(S2CellId.begin(1), S2CellId.begin(1).next()).map(cell => s"(<${s2CellIRI(cell)}>)").mkString(" ")
    def matches = restricted(pattern(p = WithinNode, g = Graphs(5)), s"(?o) { $parents }")
    matches.exact shouldBe true
    matches.positions.size shouldBe 2 * 256
    val all = matches.positions.iterator.flatten.toVector
    all should have size 512
    matches.positions.drop(300).iterator.flatten.toVector shouldBe all.drop(300)
  }

  it should "only answer quads in a graph a binding names" in {
    // a binding is a GRAPH ?g binding, which never names the union or the unnamed graph
    val graphs = Seq(Graphs(13).getURI(), "urn:x-kgf:union", "urn:ldf:defaultGraph", "urn:x-kgf:unnamed", Graphs(12).getURI())
    val values = s"(?g) { ${graphs.map(graph => s"(<$graph>)").mkString(" ")} (\"level 13\") }"
    restricted(pattern(s = yorkCell), values).positions.iterator.flatten.toSet shouldBe matching(
      pattern(s = yorkCell, g = Graphs(13))
    ).iterator.toSet
  }

  it should "restrict the union with bindings, naming the union after removing repeated quads" in {
    val neighbor = cellIRI(13, "5525166068399603712")
    val values = s"(?s ?o) { (<${yorkCell.getURI()}> UNDEF) (UNDEF <${neighbor.getURI()}>) }"
    val answered = restricted(pattern(p = TouchesNode, g = UnionGraph), values).positions.iterator.flatten.toVector
    answered.map(_.getGraph()).toSet shouldBe Set(UnionGraph)
    answered.map(_.asTriple()).toSet shouldBe restricted(pattern(p = TouchesNode), values).positions.iterator.flatten
      .map(_.asTriple())
      .toSet
    answered should have size 15
    restricted(pattern(p = TouchesNode, g = iri("urn:x-kgf:unnamed")), values).positions.size shouldBe 0
  }

  it should "answer the union by name, and nothing for graphs other than the levels and the union" in {
    for union <- Seq(UnionGraph, iri("urn:ldf:defaultGraph")) do
      matching(pattern(s = yorkCell, p = TouchesNode, g = union)).iterator.toSeq shouldBe
        matching(pattern(s = yorkCell, p = TouchesNode)).iterator.map(quad => Quad.create(UnionGraph, quad.asTriple())).toSeq
      matching(pattern(g = union)).size shouldBe matching(pattern()).size
    val deepArea = NodeFactory.createLiteralDT("999757.1006920862", XSDDatatype.XSDdouble)
    for graph <- Seq(iri("urn:x-kgf:unnamed"), iri("http://example.org/graph"), iri(s"${S2Prefix}31")) do
      withClue(graph) {
        matching(pattern(g = graph)).size shouldBe 0
        // nothing to find, rather than a pattern that can't be answered
        matching(pattern(p = HasMetricAreaNode, o = deepArea, g = graph)).size shouldBe 0
      }
  }

  it should "refuse bindings it can't answer, rather than reporting no matches" in {
    S2Graph
      .quads(pattern(p = HasMetricAreaNode), Bindings.parse(s"(?o) { (\"999757.1006920862\"^^<${XSDDatatype.XSDdouble.getURI()}>) }").get)
      .isLeft shouldBe true
  }
