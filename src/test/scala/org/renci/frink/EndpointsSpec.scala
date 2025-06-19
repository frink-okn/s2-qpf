// package org.renci.frink

// import org.renci.frink.Endpoints.{*, given}
// import org.scalatest.EitherValues
// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.should.Matchers
// import sttp.client3.*
// import sttp.client3.testing.SttpBackendStub
// import sttp.tapir.server.stub.TapirStubInterpreter

//class EndpointsSpec extends AnyFlatSpec with Matchers with EitherValues:

// it should "return hello message" in {
//   // given
//   val backendStub = TapirStubInterpreter(SttpBackendStub.synchronous)
//     .whenServerEndpointRunLogic(helloServerEndpoint)
//     .backend()

//   // when
//   val response = basicRequest
//     .get(uri"http://test.com/hello?name=adam")
//     .send(backendStub)

//   // then
//   response.body.value shouldBe "Hello adam"
// }
