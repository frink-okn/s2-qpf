package org.renci.frink.qpf

import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.graph.NodeFactory
import org.renci.frink.qpf.Types.Term
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TermSpec extends AnyFlatSpec with Matchers:

  "Term" should "parse the Hydra explicit representation" in {
    Term.parse("?s").get shouldBe Term.Variable("s")
    Term.parse("http://example.org/a").get shouldBe Term.IRI("http://example.org/a")
    Term.parse("\"abc\"").get shouldBe Term.Literal("abc", None, None)
    Term.parse("\"abc\"@en").get shouldBe Term.Literal("abc", None, Some("en"))
    Term.parse("\"5\"^^http://www.w3.org/2001/XMLSchema#integer").get shouldBe
      Term.Literal("5", Some("http://www.w3.org/2001/XMLSchema#integer"), None)
    Term.parse("\"5\"^^<http://www.w3.org/2001/XMLSchema#integer>").get shouldBe
      Term.Literal("5", Some("http://www.w3.org/2001/XMLSchema#integer"), None)
    Term.parse("\"say \"hi\"\"").get shouldBe Term.Literal("say \"hi\"", None, None)
    Term.parse("\"abc").isFailure shouldBe true
    Term.parse("\"abc\"x").isFailure shouldBe true
    Term.parse("_:b0").isFailure shouldBe true
    Term.parse("\"hello\"@en_US").isFailure shouldBe true
    Term.parse("\"hello\"@en-US").get shouldBe Term.Literal("hello", None, Some("en-US"))
  }

  it should "round trip literal nodes" in {
    val nodes = Seq(
      NodeFactory.createLiteralString("S2 Cell at level 13 with ID 5525166171478818816"),
      NodeFactory.createLiteralLang("hello", "en"),
      NodeFactory.createLiteralDT("5525166171478818816", XSDDatatype.XSDinteger),
      NodeFactory.createURI("http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level13.5525166171478818816")
    )
    for node <- nodes do Term.toNode(Term.parse(Term.encode(Term.fromNode(node))).get) shouldBe node
  }
