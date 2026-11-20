package org.renci.frink.kgf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import java.nio.file.Paths

/** This dataset's release is the release of the program that computes it, so the two places that state it must agree */
class ServiceSpec extends AnyFlatSpec with Matchers:

  "The release" should "be the version project.scala publishes" in {
    val directives = String(Files.readAllBytes(Paths.get("project.scala")), "UTF-8")
    val published = """using publish.version (\S+)""".r.findFirstMatchIn(directives).map(_.group(1))
    published shouldBe Some(Service.Release)
    Service.Version shouldBe s"v${Service.Release}"
  }

  it should "have a content digest over the code this process runs" in {
    Service.ContentDigest should fullyMatch regex "sha256:[0-9a-f]{64}"
  }

  "The caps" should "set the level an area lookup is refused below" in {
    // the deepest level whose cells fit the published candidate budget
    val fitting = Model.Levels.filter(level => Model.cells(level) <= Limits.CandidateBudget)
    fitting.max shouldBe org.renci.frink.s2.S2Graph.MaxAreaSearchLevel
    Model.cells(org.renci.frink.s2.S2Graph.MaxAreaSearchLevel + 1) > BigInt(Limits.CandidateBudget) shouldBe true
  }
