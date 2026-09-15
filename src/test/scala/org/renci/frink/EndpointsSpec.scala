package org.renci.frink

import org.apache.jena.graph.NodeFactory
import org.apache.jena.riot.Lang
import org.apache.jena.riot.RDFParser
import org.apache.jena.sparql.core.Quad
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client3.*
import sttp.client3.testing.SttpBackendStub
import sttp.model.StatusCode
import sttp.model.Uri
import sttp.tapir.server.stub.TapirStubInterpreter

import scala.jdk.CollectionConverters.*

class EndpointsSpec extends AnyFlatSpec with Matchers:

  private val backend = TapirStubInterpreter(SttpBackendStub.synchronous)
    .whenServerEndpointRunLogic(Endpoints.qpfServerEndpoint)
    .backend()

  private def get(params: (String, String)*) = request(uri"http://test.com/qpf?$params")
  private def request(url: Uri) = basicRequest.get(url).header("Accept", "application/n-quads").send(backend)
  private def quads(body: String) = RDFParser.fromString(body, Lang.NQUADS).toDatasetGraph().find().asScala.toSeq
  private def node(iri: String) = NodeFactory.createURI(iri)
  private val voidSubset = node("http://rdfs.org/ns/void#subset")
  private val hydraNext = node("http://www.w3.org/ns/hydra/core#next")
  private val hydraPrevious = node("http://www.w3.org/ns/hydra/core#previous")
  private val hydraTotalItems = node("http://www.w3.org/ns/hydra/core#totalItems")
  private val sfTouches = "http://stko-kwg.geog.ucsb.edu/lod/ontology/sfTouches"
  private val sfWithin = "http://stko-kwg.geog.ucsb.edu/lod/ontology/sfWithin"
  private val level5 = "http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level5"
  private val level1Cells = Seq("s2.level1.288230376151711744", "s2.level1.864691128455135232")
    .map(cell => s"<http://stko-kwg.geog.ucsb.edu/lod/resource/$cell>")

  private val yorkCell = "http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level13.5525166171478818816"
  private val hasMetricArea = "http://www.opengis.net/ont/geosparql#hasMetricArea"

  "The QPF endpoint" should "answer patterns with literal objects" in {
    val response = get(
      "predicate" -> "http://stko-kwg.geog.ucsb.edu/lod/ontology/cellID",
      "object" -> "\"5525166171478818816\"^^http://www.w3.org/2001/XMLSchema#integer"
    )
    response.code shouldBe StatusCode.Ok
    response.body.toOption.get should include(s"<$yorkCell>")
  }

  it should "reject malformed parameters" in {
    get("object" -> "\"hello\"@en_US").code shouldBe StatusCode.BadRequest
    get("subject" -> "\"hello\"").code shouldBe StatusCode.BadRequest
  }

  it should "refuse patterns it can't answer, rather than reporting no matches" in {
    val response = get("predicate" -> hasMetricArea, "object" -> "\"999757.1006920862\"^^http://www.w3.org/2001/XMLSchema#double")
    response.code shouldBe StatusCode.NotImplemented
    response.body.left.toOption.get should include("level 13")
  }

  it should "answer only the quads compatible with bindings, describing the page by the URL requested" in {
    val url =
      uri"http://test.com/qpf?subject=?s&predicate=$sfWithin&object=?o&graph=$level5&values=${s"(?o ?other) { (${level1Cells(0)} UNDEF) (${level1Cells(1)} 1) }"}"
    val response = request(url)
    response.code shouldBe StatusCode.Ok
    val found = quads(response.body.toOption.get)
    val (metadata, data) = found.partition(_.getGraph().getURI().endsWith("#metadata"))
    data.map(_.getObject()).toSet.subsetOf(level1Cells.map(cell => node(cell.stripPrefix("<").stripSuffix(">"))).toSet) shouldBe true
    data.map(_.getPredicate()).toSet shouldBe Set(node(sfWithin))
    data should have size 100
    val page = s"http://localhost:8080/qpf?${url.toString.dropWhile(_ != '?').drop(1)}"
    // the page is the only resource the fragment has as a subset, and it's how clients find the metadata
    metadata.filter(_.getObject() == node(page)).map(_.getPredicate()).toSet should contain(voidSubset)
    metadata.count(quad => quad.getPredicate() == voidSubset && quad.getObject() == node(page)) shouldBe 1
    val next = metadata.find(quad => quad.getSubject() == node(page) && quad.getPredicate() == hydraNext).get.getObject().getURI()
    next shouldBe s"$page&page=2"

    val secondPage = quads(request(Uri.unsafeParse(next.replace("localhost:8080", "test.com"))).body.toOption.get)
    secondPage.filter(_.getPredicate() == hydraPrevious).map(_.getObject().getURI()) shouldBe Seq(s"$page&page=1")
    secondPage.count(_.getPredicate().getURI() == sfWithin) shouldBe 100
  }

  it should "reject malformed bindings, and ignore empty ones" in {
    get("object" -> "?o", "values" -> "(?o) { (<http://example.org/a>").code shouldBe StatusCode.BadRequest
    get("object" -> "?o", "values" -> "(?o) { (<http://example.org/a>) } } LIMIT 1 VALUES ?x {").code shouldBe StatusCode.BadRequest
    val empty = get("subject" -> yorkCell, "values" -> "  ")
    empty.code shouldBe StatusCode.Ok
    empty.body.toOption.get should include(s"<$yorkCell>")
  }

  it should "fill the HTML form with bindings that can be submitted again" in {
    def html(params: (String, String)*) =
      val response = basicRequest.get(uri"http://test.com/qpf?$params").header("Accept", "text/html").send(backend)
      response.code shouldBe StatusCode.Ok
      response.body.toOption.get
    def values(page: String) =
      val escaped = "(?s)<textarea[^>]*>(.*?)</textarea>".r.findFirstMatchIn(page).get.group(1)
      Seq("&lt;" -> "<", "&gt;" -> ">", "&quot;" -> "\"", "&#39;" -> "'", "&amp;" -> "&").foldLeft(escaped) { case (text, (entity, char)) =>
        text.replace(entity, char)
      }
    val submitted =
      s"(?p ?o) { (<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> UNDEF) (UNDEF \"999757.1006920862\"^^<http://www.w3.org/2001/XMLSchema#double>) }"
    val filled = values(html("subject" -> yorkCell, "predicate" -> "?p", "object" -> "?o", "values" -> submitted))
    val resubmitted = html("subject" -> yorkCell, "predicate" -> "?p", "object" -> "?o", "values" -> filled)
    resubmitted should include("22-rdf-syntax-ns#type")
    resubmitted should include("999757.1006920862")
  }

  it should "declare the union as the default graph where Comunica reads it" in {
    val found = quads(get().body.toOption.get)
    val declared = found.filter(_.getPredicate().getURI() == "http://www.w3.org/ns/sparql-service-description#defaultGraph")
    declared.map(_.getObject().getURI()) shouldBe Seq("urn:x-kgf:union")
    declared.foreach(_.getSubject().isBlank() shouldBe true)
    found.exists(quad =>
      quad.getSubject() == node("http://localhost:8080/qpf#dataset") &&
        quad.getPredicate() == node("http://www.w3.org/ns/sparql-service-description#defaultDataset") &&
        quad.getObject() == declared.head.getSubject()
    ) shouldBe true
  }

  it should "answer the union with triples named by it, and other graphs with nothing" in {
    def answer(graph: String) =
      val found = quads(get("subject" -> yorkCell, "predicate" -> sfTouches, "graph" -> graph).body.toOption.get)
      val total = found.find(_.getPredicate() == hydraTotalItems).get.getObject().getLiteralLexicalForm().toInt
      (found.filter(_.getPredicate().getURI() == sfTouches), total)
    val (union, unionTotal) = answer("urn:x-kgf:union")
    union.map(_.getGraph().getURI()).toSet shouldBe Set("urn:x-kgf:union")
    union should have size 8
    unionTotal shouldBe 8
    val (level, _) = answer("http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level13")
    level.map(_.asTriple()).toSet shouldBe union.map(_.asTriple()).toSet
    for graph <- Seq("urn:x-kgf:unnamed", "http://example.org/graph") do answer(graph) shouldBe (Seq.empty, 0)
  }
