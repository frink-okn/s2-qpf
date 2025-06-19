package org.renci.frink

import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.apache.jena.sparql.core.DatasetGraph
import org.apache.jena.sparql.core.Quad
import org.apache.jena.sparql.core.mem.DatasetGraphInMemory
import org.renci.frink.qpf.QuadPatternFragment
import org.renci.frink.qpf.QuadPatternFragment.Parameters
import org.renci.frink.qpf.Types.ApplicationNQuads
import org.renci.frink.qpf.Types.ApplicationTrig
import org.renci.frink.qpf.Types.DatasetGraphUtils
import org.renci.frink.qpf.Types.VariableOrIRI
import org.renci.frink.qpf.Types.VariableOrIRI.*
import org.renci.frink.s2.S2Graph
import sttp.model.Uri
import sttp.model.Uri.*
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint

import java.util.UUID
import scala.jdk.CollectionConverters.*

object Endpoints:
  case class User(name: String) extends AnyVal
  val qpfEndpoint: PublicEndpoint[
    (Option[VariableOrIRI], Option[VariableOrIRI], Option[VariableOrIRI], Option[VariableOrIRI], Option[BigInt]),
    Unit,
    QuadPatternFragment,
    Any
  ] =
    endpoint.get
      .in("qpf")
      .in(query[Option[VariableOrIRI]]("subject"))
      .in(query[Option[VariableOrIRI]]("predicate"))
      .in(query[Option[VariableOrIRI]]("object"))
      .in(query[Option[VariableOrIRI]]("graph"))
      .in(
        query[Option[BigInt]]("page")
          .validateOption(Validator.positive[BigInt])
      )
      .out(
        oneOfBody[QuadPatternFragment](
          stringBodyUtf8AnyFormat(DatasetGraphUtils.trigFormat),
          stringBodyUtf8AnyFormat(DatasetGraphUtils.nQuadsFormat),
          stringBodyUtf8AnyFormat(DatasetGraphUtils.jsonldFormat),
          stringBodyUtf8AnyFormat(DatasetGraphUtils.htmlFormat)
        )
      )

  val qpfServerEndpoint: ServerEndpoint[Any, Identity] =
    def toNode(term: Option[VariableOrIRI]): Node =
      term match
        case Some(Variable(value)) => NodeFactory.createVariable(value)
        case Some(IRI(value))      => NodeFactory.createURI(value)
        case None                  => NodeFactory.createVariable(UUID.randomUUID().toString())
    val location = sys.env.get("QPF_SERVER_LOCATION").flatMap(s => Uri.parse(s).toOption).getOrElse(uri"http://localhost:8080")
    val service = location.addPath("qpf")
    qpfEndpoint.handleSuccess { case (s, p, o, g, page) =>
      val filteredS = s.filterNot(_.value.isBlank())
      val filteredP = p.filterNot(_.value.isBlank())
      val filteredO = o.filterNot(_.value.isBlank())
      val filteredG = g.filterNot(_.value.isBlank())
      val quadPattern = Quad.create(toNode(filteredG), toNode(filteredS), toNode(filteredP), toNode(filteredO))
      QuadPatternFragment.qpf(
        S2Graph.quads(quadPattern),
        Parameters(filteredS, filteredP, filteredO, filteredG, page),
        Map(
          "kwg" -> "http://stko-kwg.geog.ucsb.edu/lod/resource/",
          "" -> "http://stko-kwg.geog.ucsb.edu/lod/ontology/"
        ),
        service
      )
    }

  val apiEndpoints: List[ServerEndpoint[Any, Identity]] = List(qpfServerEndpoint)

  val all: List[ServerEndpoint[Any, Identity]] = apiEndpoints
