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

  "The prefix map" should "declare what the OKN shared prefix table declares" in {
    val table = Paths.get("../okn-registry/docs/registry/prefixes.yaml")
    assume(Files.exists(table), "the okn-registry checkout is beside this one")
    val entry = """^([A-Za-z0-9_.-]+):\s*"([^"]+)"""".r
    val shared = String(Files.readAllBytes(table), "UTF-8").linesIterator
      .flatMap(entry.findFirstMatchIn)
      .map(found => found.group(1) -> found.group(2))
      .toMap
    for (prefix, namespace) <- Service.Prefixes do withClue(s"$prefix: ") { shared.get(prefix) shouldBe Some(namespace) }
    // hydra is the TPF document's own hypermedia vocabulary, which no graph of the federation publishes
    for (prefix, namespace) <- org.renci.frink.qpf.QuadPatternFragment.ControlPrefixes - "hydra" do
      withClue(s"$prefix: ") { shared.get(prefix) shouldBe Some(namespace) }
  }

  "The caps" should "set the level an area lookup is refused below" in {
    // the deepest level whose cells fit the published candidate budget
    val fitting = Model.Levels.filter(level => Model.cells(level) <= Limits.CandidateBudget)
    fitting.max shouldBe org.renci.frink.s2.S2Graph.MaxAreaSearchLevel
    Model.cells(org.renci.frink.s2.S2Graph.MaxAreaSearchLevel + 1) > BigInt(Limits.CandidateBudget) shouldBe true
  }
