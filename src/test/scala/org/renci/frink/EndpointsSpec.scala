package org.renci.frink

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sttp.client3.*
import sttp.client3.testing.SttpBackendStub
import sttp.model.StatusCode
import sttp.tapir.server.stub.TapirStubInterpreter

class EndpointsSpec extends AnyFlatSpec with Matchers:

  private val backend = TapirStubInterpreter(SttpBackendStub.synchronous)
    .whenServerEndpointRunLogic(Endpoints.qpfServerEndpoint)
    .backend()

  private def get(params: (String, String)*) =
    basicRequest.get(uri"http://test.com/qpf?$params").header("Accept", "application/n-quads").send(backend)

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
