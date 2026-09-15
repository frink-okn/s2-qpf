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
import org.renci.frink.qpf.Types.Term
import org.renci.frink.s2.S2Graph
import sttp.model.StatusCode
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
    (Option[Term], Option[Term], Option[Term], Option[Term], Option[BigInt]),
    String,
    QuadPatternFragment,
    Any
  ] =
    endpoint.get
      .in("qpf")
      .in(query[Option[Term]]("subject").validateOption(Term.notLiteral))
      .in(query[Option[Term]]("predicate").validateOption(Term.notLiteral))
      .in(query[Option[Term]]("object"))
      .in(query[Option[Term]]("graph").validateOption(Term.notLiteral))
      .in(
        query[Option[BigInt]]("page")
          .validateOption(Validator.positive[BigInt])
      )
      // patterns the server can't answer, rather than claiming they have no matches
      .errorOut(statusCode(StatusCode.NotImplemented).and(stringBody))
      .out(
        oneOfBody[QuadPatternFragment](
          stringBodyUtf8AnyFormat(DatasetGraphUtils.trigFormat),
          stringBodyUtf8AnyFormat(DatasetGraphUtils.nQuadsFormat),
          stringBodyUtf8AnyFormat(DatasetGraphUtils.jsonldFormat),
          stringBodyUtf8AnyFormat(DatasetGraphUtils.htmlFormat)
        )
      )

  val qpfServerEndpoint: ServerEndpoint[Any, Identity] =
    def toNode(term: Option[Term]): Node =
      term.map(Term.toNode).getOrElse(NodeFactory.createVariable(UUID.randomUUID().toString()))
    val location = sys.env.get("QPF_SERVER_LOCATION").flatMap(s => Uri.parse(s).toOption).getOrElse(uri"http://localhost:8080")
    val service = location.addPath("qpf")
    qpfEndpoint.handle { case (s, p, o, g, page) =>
      val filteredS = s.filterNot(_.isBlank)
      val filteredP = p.filterNot(_.isBlank)
      val filteredO = o.filterNot(_.isBlank)
      val filteredG = g.filterNot(_.isBlank)
      val quadPattern = Quad.create(toNode(filteredG), toNode(filteredS), toNode(filteredP), toNode(filteredO))
      S2Graph
        .quads(quadPattern)
        .map(quads =>
          QuadPatternFragment.qpf(
            quads,
            Parameters(filteredS, filteredP, filteredO, filteredG, page),
            Map(
              "kwg" -> S2Graph.KWGResource,
              "" -> S2Graph.KWGOnt,
              "geo" -> S2Graph.GeoSPARQL,
              "spatial-full" -> S2Graph.SpatialFull
            ),
            service
          )
        )
    }

  val apiEndpoints: List[ServerEndpoint[Any, Identity]] = List(qpfServerEndpoint)

  val all: List[ServerEndpoint[Any, Identity]] = apiEndpoints
