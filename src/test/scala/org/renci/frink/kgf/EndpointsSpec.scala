package org.renci.frink.kgf

import org.apache.jena.atlas.json.JSON
import org.apache.jena.atlas.json.JsonObject
import org.apache.jena.atlas.json.JsonValue
import org.apache.jena.riot.Lang
import org.apache.jena.riot.RDFParser
import org.apache.jena.sparql.core.Quad
import org.renci.frink.s2.S2Graph
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client3.*
import sttp.client3.testing.SttpBackendStub
import sttp.model.Method
import sttp.model.StatusCode
import sttp.model.Uri
import sttp.tapir.server.stub.TapirStubInterpreter

import scala.jdk.CollectionConverters.*

/** The KGF surface over the stub server: the envelopes, the four forms of `g`, paging by cursor, bindings, the refusal, and the coded
  * errors
  */
class EndpointsSpec extends AnyFlatSpec with Matchers:

  private val backend = TapirStubInterpreter(SttpBackendStub.synchronous)
    .whenServerEndpointsRunLogic(org.renci.frink.Endpoints.all)
    .backend()

  private val version = Service.Version
  private val cell = "http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level13.5525166171478818816"
  private val level13 = "http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level13"
  private val sfTouches = "http://stko-kwg.geog.ucsb.edu/lod/ontology/sfTouches"
  private val cellID = "http://stko-kwg.geog.ucsb.edu/lod/ontology/cellID"
  private val hasMetricArea = "http://www.opengis.net/ont/geosparql#hasMetricArea"

  private def send(request: Request[Either[String, String], Any]) = request.send(backend)

  private def at(operation: String, params: (String, String)*): Uri =
    uri"http://test.com/s2/v/$version/$operation?$params"

  private def get(operation: String, params: (String, String)*) = send(basicRequest.get(at(operation, params*)))

  private def json(operation: String, params: (String, String)*): JsonObject =
    val response = get(operation, params*)
    withClue(s"$operation ${params.mkString(" ")} -> ${response.code}: ${response.body.fold(identity, identity).take(400)}\n") {
      response.code shouldBe StatusCode.Ok
    }
    JSON.parse(response.body.toOption.get)

  private def problem(response: Response[Either[String, String]]): JsonObject =
    JSON.parse(response.body.left.toOption.getOrElse(response.body.toOption.get))

  private def code(response: Response[Either[String, String]]): String = problem(response).get("code").getAsString.value()

  private def number(value: JsonValue): BigInt = BigInt(value.getAsNumber.value().toString.takeWhile(_ != '.'))

  private def rows(answer: JsonObject): Seq[JsonObject] = answer.get("rows").getAsArray.asScala.toSeq.map(_.getAsObject)

  private def quads(body: String) = RDFParser.fromString(body, Lang.NQUADS).toDatasetGraph().find().asScala.toSeq

  "The service descriptor" should "name this dataset, its capabilities, its caps and its budgets" in {
    val answer = JSON.parse(send(basicRequest.get(uri"http://test.com/")).body.toOption.get)
    val dataset = answer.get("datasets").getAsArray.get(0).getAsObject
    dataset.get("id").getAsString.value() shouldBe "s2"
    dataset.get("class").getAsString.value() shouldBe "computed"
    number(dataset.get("triples")) shouldBe BigInt("1361984604108888559446")
    dataset.get("capabilities").getAsArray.asScala.map(_.getAsString.value()) should contain theSameElementsAs Seq("graphs", "terms")
    answer.get("caps").getAsObject.get("max_limit").getAsNumber.value().intValue() shouldBe Limits.MaxLimit
    answer.get("budgets").getAsObject.get("candidate_budget").getAsNumber.value().intValue() shouldBe Limits.CandidateBudget
  }

  it should "describe the dataset and its one release" in {
    val answer = JSON.parse(send(basicRequest.get(uri"http://test.com/s2")).body.toOption.get)
    answer.get("current").getAsString.value() shouldBe version
    val release = answer.get("releases").getAsArray.get(0).getAsObject
    release.get("content_digest").getAsString.value() should startWith("sha256:")
    release.get("links").getAsObject.get("fragment").getAsString.value() shouldBe s"/s2/v/$version/fragment"
  }

  it should "redirect `latest` without rewriting the method" in {
    val response = send(basicRequest.get(uri"http://test.com/s2/latest/fragment?s=x"))
    response.code shouldBe StatusCode.TemporaryRedirect
    response.header("Location") shouldBe Some(s"/s2/v/$version/fragment?s=x")
    val redirected = send(basicRequest.post(uri"http://test.com/s2/latest/count").body("{}"))
    redirected.code shouldBe StatusCode.TemporaryRedirect
  }

  it should "publish a manifest whose artifact is the generator" in {
    val answer = json("manifest")
    answer.get("version").getAsString.value() shouldBe version
    answer.get("class").getAsString.value() shouldBe "computed"
    val generator = answer.get("artifacts").getAsObject.get("generator").getAsObject
    generator.get("release").getAsString.value() shouldBe Service.Release
    generator.get("image").getAsString.value() should include("s2-qpf")
    answer.get("prefixes").getAsObject.get("kwg-s2").getAsString.value() should endWith("s2.")
  }

  "A fragment" should "answer a bound subject with an exact cardinality" in {
    val answer = json("fragment", "s" -> s"<$cell>")
    answer.get("cardinality").getAsObject.get("exact").getAsBoolean.value() shouldBe true
    answer.get("vars").getAsArray.asScala.map(_.getAsString.value()) shouldBe Seq("p", "o")
    rows(answer).head.keys().asScala should contain theSameElementsAs Seq("p", "o")
  }

  it should "take a CURIE against the declared prefix map" in {
    val curie = json("fragment", "s" -> s"<$cell>", "p" -> "kwg-ont:sfTouches")
    val iri = json("fragment", "s" -> s"<$cell>", "p" -> s"<$sfTouches>")
    number(curie.get("cardinality").getAsObject.get("value")) shouldBe BigInt(8)
    Json.write(Json.Str(curie.toString)) shouldBe Json.write(Json.Str(iri.toString))
  }

  it should "refuse a term that is neither bracketed nor a declared CURIE" in {
    code(get("fragment", "s" -> cell)) shouldBe "bad_term_syntax"
    code(get("fragment", "p" -> "nosuch:thing")) shouldBe "bad_term_syntax"
    code(get("fragment", "s" -> "\"a literal\"")) shouldBe "malformed_request"
  }

  it should "answer the four forms of g" in {
    def total(params: (String, String)*) = number(json("count", params*).get("count").getAsObject.get("value"))
    val union = total("s" -> s"<$cell>", "p" -> s"<$sfTouches>")
    union shouldBe BigInt(8)
    total("s" -> s"<$cell>", "p" -> s"<$sfTouches>", "g" -> "<urn:x-kgf:union>") shouldBe union
    total("s" -> s"<$cell>", "p" -> s"<$sfTouches>", "g" -> s"<$level13>") shouldBe union
    total("s" -> s"<$cell>", "p" -> s"<$sfTouches>", "g" -> "*") shouldBe union
    total("s" -> s"<$cell>", "p" -> s"<$sfTouches>", "g" -> "<urn:x-kgf:unnamed>") shouldBe BigInt(0)
    total("s" -> s"<$cell>", "p" -> s"<$sfTouches>", "g" -> "<http://example.org/g>") shouldBe BigInt(0)
  }

  it should "add a g column in the quad view, and name the level graph in it" in {
    val answer = json("fragment", "s" -> s"<$cell>", "p" -> s"<$sfTouches>", "g" -> "*")
    answer.get("vars").getAsArray.asScala.map(_.getAsString.value()) shouldBe Seq("o", "g")
    rows(answer).map(_.get("g").getAsObject.get("value").getAsString.value()).distinct shouldBe Seq(level13)
  }

  it should "write the union untagged and the quad view tagged, and refuse the quad view in Turtle" in {
    val untagged = get("fragment", "s" -> s"<$cell>", "p" -> s"<$sfTouches>", "format" -> "nq")
    quads(untagged.body.toOption.get).map(quad => Quad.isDefaultGraph(quad.getGraph())).distinct shouldBe Seq(true)
    val tagged = get("fragment", "s" -> s"<$cell>", "p" -> s"<$sfTouches>", "g" -> "*", "format" -> "nq")
    quads(tagged.body.toOption.get).map(_.getGraph().getURI()).distinct shouldBe Seq(level13)
    val refused = get("fragment", "s" -> s"<$cell>", "g" -> "*", "format" -> "ttl")
    refused.code shouldBe StatusCode.NotAcceptable
    code(refused) shouldBe "not_acceptable"
  }

  it should "page by cursor, losing and repeating nothing" in {
    val whole = rows(json("fragment", "s" -> s"<$cell>", "p" -> s"<$sfTouches>"))
    whole should have size 8
    val first = json("fragment", "s" -> s"<$cell>", "p" -> s"<$sfTouches>", "limit" -> "3")
    first.get("complete").getAsBoolean.value() shouldBe false
    first.get("truncation_reason").getAsString.value() shouldBe "page_limit"
    val cursor = first.get("next").getAsString.value()
    val second = json("fragment", "s" -> s"<$cell>", "p" -> s"<$sfTouches>", "limit" -> "3", "cursor" -> cursor)
    rows(first).map(_.toString) ++ rows(second).map(_.toString) shouldBe whole.take(6).map(_.toString)
    // a cursor is bound to the request it is a position into
    code(get("fragment", "s" -> s"<$cell>", "cursor" -> cursor)) shouldBe "stale_cursor"
    code(get("fragment", "s" -> s"<$cell>", "cursor" -> "not-a-cursor")) shouldBe "stale_cursor"
  }

  it should "page a fragment that no store could hold, at any offset" in {
    val level30 = json("count", "o" -> "<http://stko-kwg.geog.ucsb.edu/lod/ontology/S2Cell_Level30>", "p" -> "rdf:type")
    number(level30.get("count").getAsObject.get("value")) shouldBe Model.cells(30)
    val pattern = Operations.PatternRequest
      .parse(None, Some("rdf:type"), Some("<http://stko-kwg.geog.ucsb.edu/lod/ontology/S2Cell_Level30>"), None, None, None)
      .toOption
      .get
    val deep = Cursor.encode("fragment", s"${pattern.canonical}\n0", BigInt("1000000000000000000"))
    val answer = json(
      "fragment",
      "p" -> "rdf:type",
      "o" -> "<http://stko-kwg.geog.ucsb.edu/lod/ontology/S2Cell_Level30>",
      "limit" -> "2",
      "cursor" -> deep
    )
    rows(answer) should have size 2
    answer.get("complete").getAsBoolean.value() shouldBe false
  }

  it should "enforce the published caps" in {
    code(get("fragment", "limit" -> (Limits.MaxLimit + 1).toString)) shouldBe "cap_exceeded"
    code(get("fragment", "format" -> "parquet")) shouldBe "unsupported_format"
  }

  it should "report a capability it does not declare, and a pattern it cannot answer" in {
    code(get("fragment", "o.text" -> "atrazine")) shouldBe "capability_not_available"
    code(get("fragment", "o.ge" -> "\"1\"^^xsd:double")) shouldBe "capability_not_available"
    val refused = get("fragment", "p" -> s"<$hasMetricArea>", "o" -> "\"999757.1006920862\"^^xsd:double")
    refused.code shouldBe StatusCode.NotImplemented
    code(refused) shouldBe "pattern_not_supported"
    problem(refused).get("detail").getAsString.value() should include("level 13")
  }

  it should "answer a datatype restriction exactly, and a language tag with nothing" in {
    val integers = json("fragment", "s" -> s"<$cell>", "o.dt" -> "xsd:integer")
    rows(integers).map(_.get("p").getAsObject.get("value").getAsString.value()).distinct shouldBe Seq(cellID)
    number(json("count", "s" -> s"<$cell>", "o.lang" -> "en").get("count").getAsObject.get("value")) shouldBe BigInt(0)
  }

  "Bindings" should "answer one row per input row and quad, with an exact total" in {
    val body =
      s"""{"pattern": {"s": "?cell", "p": "<$sfTouches>", "o": "?n"},
         | "bindings": {"vars": ["?cell"], "rows": [["<$cell>"], ["<$level13>"], [null]]}}""".stripMargin
    for method <- Seq(Method.POST, Method.unsafeApply("QUERY")) do
      val response = send(basicRequest.method(method, at("fragment")).body(body))
      withClue(s"$method: ${response.body.fold(identity, identity).take(300)}\n") { response.code shouldBe StatusCode.Ok }
      val answer = JSON.parse(response.body.toOption.get)
      // row 0 is the cell's eight neighbours, row 1 is a graph IRI and names no cell, row 2 is UNDEF and matches every quad of the pattern
      rows(answer).map(_.get("binding").getAsNumber.value().intValue()).distinct shouldBe Seq(0, 2)
      answer.get("cardinality").getAsObject.get("exact").getAsBoolean.value() shouldBe true
      rows(answer).count(_.get("binding").getAsNumber.value().intValue() == 0) shouldBe 8
  }

  it should "count each input row" in {
    val body = s"""{"pattern": {"s": "?cell", "p": "<$sfTouches>", "o": "?n"},
                  | "bindings": {"vars": ["?cell"], "rows": [["<$cell>"], ["<http://example.org/x>"]]}}""".stripMargin
    val answer = JSON.parse(send(basicRequest.post(at("count")).body(body)).body.toOption.get)
    val counts = answer.get("counts").getAsArray.asScala.map(_.getAsObject).toSeq
    counts.map(entry => entry.get("binding").getAsNumber.value().intValue() -> number(entry.get("count").getAsObject.get("value"))) shouldBe
      Seq(0 -> BigInt(8), 1 -> BigInt(0))
  }

  it should "refuse a body field it does not know, and a table over the cap" in {
    val unknown = send(basicRequest.post(at("count")).body("""{"pattern": {"s": "?s"}, "limit": 5}"""))
    code(unknown) shouldBe "malformed_request"
    val rows = (1 to Limits.MaxBindings + 1).map(index => s"""["<http://example.org/$index>"]""").mkString(",")
    val over = send(basicRequest.post(at("fragment")).body(s"""{"pattern": {"s": "?s"}, "bindings": {"vars": ["?s"], "rows": [$rows]}}"""))
    code(over) shouldBe "cap_exceeded"
  }

  "Describe" should "read a resource in both directions" in {
    val both = json("describe", "iri" -> s"<$cell>")
    val out = json("describe", "iri" -> s"<$cell>", "direction" -> "out")
    val in = json("describe", "iri" -> s"<$cell>", "direction" -> "in")
    number(both.get("cardinality").getAsObject.get("value")) shouldBe
      number(out.get("cardinality").getAsObject.get("value")) + number(in.get("cardinality").getAsObject.get("value"))
    both.get("direction").getAsString.value() shouldBe "both"
  }

  "Graphs" should "list one graph per level, with counts that sum to the dataset" in {
    val answer = json("graphs", "limit" -> "100")
    val listed = answer.get("graphs").getAsArray.asScala.map(_.getAsObject).toSeq
    listed should have size 31
    listed.map(_.get("g").getAsObject.get("value").getAsString.value()).head shouldBe S2Graph.Graphs(0).getURI()
    listed.map(entry => number(entry.get("count"))).sum shouldBe number(answer.get("triples"))
    answer.get("complete").getAsBoolean.value() shouldBe true
  }

  "Terms" should "count a prefix in closed form, by role" in {
    val answer = json("terms", "prefix" -> "http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level13.", "count" -> "true")
    number(answer.get("count").getAsObject.get("value")) shouldBe Model.cells(13)
    number(answer.get("counts").getAsObject.get("predicate")) shouldBe BigInt(0)
    val predicates = json("terms", "prefix" -> "http://stko-kwg.geog.ucsb.edu/lod/ontology/sf", "count" -> "true", "role" -> "predicate")
    number(predicates.get("count").getAsObject.get("value")) shouldBe BigInt(3)
  }

  it should "scan a prefix, paging from any position" in {
    val answer = json("terms", "prefix" -> "http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level13.", "limit" -> "3")
    val terms = answer.get("terms").getAsArray.asScala.map(_.getAsObject).toSeq
    terms should have size 3
    terms.head.get("roles").getAsArray.asScala.map(_.getAsString.value()) shouldBe Seq("object", "subject")
    answer.get("complete").getAsBoolean.value() shouldBe false
    val next = json(
      "terms",
      "prefix" -> "http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level13.",
      "limit" -> "3",
      "cursor" -> answer.get("next").getAsString.value()
    )
    next.get("terms").getAsArray.get(0).getAsObject.toString should not be terms.head.toString
  }

  "Schema" should "describe the dataset root and page its classes" in {
    val root = json("schema")
    root.get("view").getAsString.value() shouldBe "design"
    number(root.get("node").getAsObject.get("counts").getAsObject.get("triples")) shouldBe Model.Queryable.triples
    val classes = json("schema", "children" -> "classes", "limit" -> "2")
    classes.get("collection").getAsObject.get("kind").getAsString.value() shouldBe "classes"
    classes.get("items").getAsArray.size() shouldBe 2
    classes.get("complete").getAsBoolean.value() shouldBe false
  }

  it should "read a view per level graph, and refuse a view it does not have" in {
    val view = json("schema", "view" -> s"graph:$level13")
    number(view.get("node").getAsObject.get("counts").getAsObject.get("triples")) shouldBe Model.levelTriples(13)
    code(get("schema", "view" -> "graph:http://example.org/g")) shouldBe "not_found"
    code(get("schema", "children" -> "languages")) shouldBe "malformed_request"
  }

  it should "page the flat projections in triples order" in {
    val relations = json("schema", "projection" -> "class-relations", "limit" -> "5")
    val items = relations.get("items").getAsArray.asScala.map(_.getAsObject).toSeq
    items.map(item => number(item.get("triples"))) shouldBe items.map(item => number(item.get("triples"))).sorted.reverse
    relations.get("order").getAsObject.get("by").getAsString.value() shouldBe "triples"
    val properties = json("schema", "projection" -> "class-properties", "limit" -> "3")
    properties.get("items").getAsArray.size() shouldBe 3
  }

  "The description surface" should "serialize VoID, and state the dataset's own total in it" in {
    val response = get("void")
    response.code shouldBe StatusCode.Ok
    val model = RDFParser.fromString(response.body.toOption.get, Lang.TURTLE).toGraph()
    val triples = model
      .find(
        org.apache.jena.graph.NodeFactory.createURI(Service.DatasetIRI),
        org.apache.jena.graph.NodeFactory.createURI("http://rdfs.org/ns/void#triples"),
        org.apache.jena.graph.Node.ANY
      )
      .asScala
      .toSeq
    triples.map(_.getObject().getLiteralLexicalForm()) shouldBe Seq(Model.Queryable.triples.toString)
  }

  it should "offer the summary card as Markdown by default, and as JSON on request" in {
    val card = get("summary")
    card.body.toOption.get should startWith(s"# ${Service.Title}")
    card.body.toOption.get should include("geo:hasMetricArea")
    val answer = json("summary", "format" -> "json")
    number(answer.get("counts").getAsObject.get("triples")) shouldBe Model.Queryable.triples
  }

  it should "offer an HTML view of every operation" in {
    for operation <- Seq("fragment", "count", "graphs", "schema", "summary", "manifest", "void") do
      val response = send(basicRequest.get(at(operation, "format" -> "html", "iri" -> s"<$cell>", "prefix" -> "http://x")))
      withClue(s"$operation: ") { response.code shouldBe StatusCode.Ok }
      response.body.toOption.get should startWith("<!DOCTYPE html>")
  }

  "The server" should "answer an unknown dataset, release or resource with a coded 404" in {
    code(send(basicRequest.get(uri"http://test.com/other"))) shouldBe "not_found"
    code(send(basicRequest.get(uri"http://test.com/s2/v/v0.0.1/fragment"))) shouldBe "not_found"
    code(send(basicRequest.get(uri"http://test.com/s2/v/$version/nosuch"))) shouldBe "not_found"
  }

  it should "name the methods a resource takes when the method is not one of them" in {
    val response = send(basicRequest.put(at("fragment")).body(""))
    response.code shouldBe StatusCode.MethodNotAllowed
    code(response) shouldBe "method_not_allowed"
    response.header("Allow") shouldBe Some("GET, QUERY, POST, HEAD")
  }

  it should "carry the completeness vocabulary in headers as well as in the envelope" in {
    val response = get("fragment", "s" -> s"<$cell>", "p" -> s"<$sfTouches>", "limit" -> "3")
    response.header("KGF-Complete") shouldBe Some("false")
    response.header("KGF-Truncation-Reason") shouldBe Some("page_limit")
    response.header("KGF-Next-Cursor") should not be None
    response.header("Cache-Control") shouldBe Some("public, max-age=31536000, immutable")
    response.header("ETag").get should include(Service.ContentDigest)
  }
