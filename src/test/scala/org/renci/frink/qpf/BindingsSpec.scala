package org.renci.frink.qpf

import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.apache.jena.sparql.core.Quad
import org.renci.frink.Util.SizedIterator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BindingsSpec extends AnyFlatSpec with Matchers:

  private def iri(value: String) = NodeFactory.createURI(s"http://example.org/$value")
  private def variable(name: String) = NodeFactory.createVariable(name)
  private def pattern(s: Node = variable("s"), p: Node = variable("p"), o: Node = variable("o"), g: Node = variable("g")) =
    Quad.create(g, s, p, o)
  private def bindings(text: String) = Bindings.parse(text).get

  "Bindings" should "parse the VALUES syntax Comunica sends" in {
    val parsed = bindings(
      """(?s ?o) { (<http://example.org/a> "x"@en ) (UNDEF "5"^^<http://www.w3.org/2001/XMLSchema#integer> ) (<http://example.org/b> 7 ) }"""
    )
    parsed.variables.map(_.getVarName()) shouldBe Seq("s", "o")
    val rows = parsed.rows.map(row => parsed.variables.map(variable => Option(row.get(variable))))
    rows shouldBe Seq(
      Seq(Some(iri("a")), Some(NodeFactory.createLiteralLang("x", "en"))),
      Seq(None, Some(NodeFactory.createLiteralDT("5", XSDDatatype.XSDinteger))),
      Seq(Some(iri("b")), Some(NodeFactory.createLiteralDT("7", XSDDatatype.XSDinteger)))
    )
    bindings(Bindings.encode(parsed)) shouldBe parsed
    bindings("?s { <http://example.org/a> }").rows should have size 1
    bindings("(?s) { }").rows shouldBe empty
  }

  it should "reject anything but a single VALUES block" in {
    val invalid = Seq(
      "",
      "(?s) { (<http://example.org/a>) ",
      "(?s ?o) { (<http://example.org/a>) }",
      "(?s) { (_:b0) }",
      // valid queries once the group pattern is closed after the text
      "(?s) { (<http://example.org/a>) } } VALUES ?x {",
      "(?s) { (<http://example.org/a>) } } ORDER BY ?s LIMIT 1 VALUES ?x {",
      "(?s) { (<http://example.org/a>) } ?s ?p ?o",
      "(?s) { (<< <http://example.org/a> <http://example.org/b> <http://example.org/c> >>) }"
    )
    for text <- invalid do withClue(text)(Bindings.parse(text).isFailure shouldBe true)
    val rows = Seq.fill(Bindings.MaxRows + 1)("(<http://example.org/a>)").mkString(" ")
    Bindings.parse(s"(?s) { $rows }").isFailure shouldBe true
  }

  it should "substitute each row into the pattern, ignoring variables the pattern doesn't have" in {
    val restricted = bindings("(?s ?other) { (<http://example.org/a> <http://example.org/x>) (<http://example.org/b> UNDEF) }")
    restricted.restrict(pattern(p = iri("p"))) shouldBe Seq(pattern(s = iri("a"), p = iri("p")), pattern(s = iri("b"), p = iri("p")))
  }

  it should "leave out restrictions that another row subsumes, whatever order the rows are in" in {
    val general = pattern(s = iri("a"))
    val specific = pattern(s = iri("a"), o = iri("b"))
    val other = pattern(o = iri("c"))
    bindings("(?s ?o) { (<http://example.org/a> <http://example.org/b>) (UNDEF <http://example.org/c>) (<http://example.org/a> UNDEF) }")
      .restrict(pattern()) shouldBe Seq(other, general)
    bindings("(?s ?o) { (<http://example.org/a> UNDEF) (<http://example.org/a> UNDEF) (<http://example.org/a> <http://example.org/b>) }")
      .restrict(pattern()) shouldBe Seq(general)
    bindings("(?s) { (UNDEF) (<http://example.org/a>) }").restrict(pattern()) shouldBe Seq(pattern())
    Bindings.subsumes(general, specific) shouldBe true
    Bindings.subsumes(specific, general) shouldBe false
  }

  it should "only subsume patterns that repeat a term wherever the general pattern repeats a variable" in {
    val repeated = pattern(s = variable("x"), o = variable("x"))
    Bindings.subsumes(repeated, pattern(s = iri("a"), o = iri("a"))) shouldBe true
    Bindings.subsumes(repeated, pattern(s = iri("a"), o = iri("b"))) shouldBe false
    Bindings.subsumes(repeated, Quad.create(iri("g"), iri("a"), iri("p"), iri("b"))) shouldBe false
  }

  it should "answer each quad matching several rows only once, at the position of the first row" in {
    val quads = for
      s <- Seq("a", "b")
      o <- Seq("c", "d")
    yield Quad.create(iri("g"), iri(s), iri("p"), iri(o))
    def find(restriction: Quad) = Right(SizedIterator.fromSeq(quads.filter(Bindings.subsumes(restriction, _))))

    val disjoint =
      bindings("(?s) { (<http://example.org/a>) (<http://example.org/b>) (<http://example.org/z>) }").matches(pattern(), find).toOption.get
    disjoint.exact shouldBe true
    disjoint.positions.iterator.toSeq shouldBe quads.map(Some(_))

    // positions can only be iterated once
    def overlapping =
      bindings("(?s ?o) { (<http://example.org/a> UNDEF) (UNDEF <http://example.org/c>) }").matches(pattern(), find).toOption.get
    overlapping.exact shouldBe false
    overlapping.positions.size shouldBe 4
    overlapping.positions.iterator.toSeq shouldBe Seq(Some(quads(0)), Some(quads(1)), None, Some(quads(2)))
    overlapping.positions.drop(2).iterator.toSeq shouldBe Seq(None, Some(quads(2)))

    bindings("(?s) { (<http://example.org/a>) }").matches(pattern(), _ => Left("can't")) shouldBe Left("can't")
  }
