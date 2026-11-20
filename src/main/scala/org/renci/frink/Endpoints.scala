package org.renci.frink

import io.netty.handler.codec.http.HttpRequest
import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.apache.jena.sparql.core.Quad
import org.renci.frink.kgf.Problem
import org.renci.frink.kgf.Service
import org.renci.frink.qpf.Bindings
import org.renci.frink.qpf.Matches
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
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint

import java.util.UUID

/** The TPF compatibility route, `/{dataset}/v/{version}/tpf` (KGF doc 03 section 3.8).
  *
  * Quad Pattern Fragments with bindings, in the Hydra explicit representation, which is what a stock Comunica `qpf` or `brtpf` source
  * speaks. Graph tagging follows `docs/graph-semantics.md`. Terms here are written bare, as Hydra's explicit representation writes them,
  * rather than in the bracketed syntax of KGF doc 03 section 3.3 that `/fragment` takes: each route keeps its own standard.
  */
object Endpoints:

  private val tpfPath = "tpf"

  val tpfEndpoint: PublicEndpoint[
    (String, String, Option[Term], Option[Term], Option[Term], Option[Term], Option[BigInt], Option[Bindings], Option[String]),
    Problem,
    QuadPatternFragment,
    Any
  ] =
    endpoint.get
      .in(path[String]("dataset") / "v" / path[String]("version") / tpfPath)
      .in(query[Option[Term]]("subject").validateOption(Term.notLiteral))
      .in(query[Option[Term]]("predicate").validateOption(Term.notLiteral))
      .in(query[Option[Term]]("object"))
      .in(query[Option[Term]]("graph").validateOption(Term.notLiteral))
      .in(
        query[Option[BigInt]]("page")
          .validateOption(Validator.positive[BigInt])
      )
      // brTPF bindings; an HTML form submits an empty field when there are none
      .in(
        query[Option[String]]("values")
          .mapDecode(_.filterNot(_.isBlank) match
            case Some(text) => Bindings.decode(text).map(Some(_))
            case None       => DecodeResult.Value(None))(_.map(Bindings.encode))
      )
      .in(extractFromRequest(requestQuery))
      // patterns the server can't answer, rather than claiming they have no matches
      .errorOut(kgf.Endpoints.problemOut)
      .out(
        oneOfBody[QuadPatternFragment](
          stringBodyUtf8AnyFormat(DatasetGraphUtils.trigFormat),
          stringBodyUtf8AnyFormat(DatasetGraphUtils.nQuadsFormat),
          stringBodyUtf8AnyFormat(DatasetGraphUtils.jsonldFormat),
          stringBodyUtf8AnyFormat(DatasetGraphUtils.htmlFormat)
        )
      )

  /** The query string as the client sent it. Decoding and encoding it again could spell it differently, and clients look for the metadata
    * about the URL they requested.
    */
  private def requestQuery(request: ServerRequest): Option[String] =
    val target = request.underlying match
      case netty: HttpRequest => netty.uri()
      case _                  => request.uri.toString
    Option(target.indexOf('?')).filter(_ >= 0).map(start => target.substring(start + 1).takeWhile(_ != '#'))

  /** Where this route's pages and search template are published */
  val tpfService: Uri = Service.PublicBase.addPath("s2", "v", Service.Version, tpfPath)

  val tpfServerEndpoint: ServerEndpoint[Any, Identity] =
    def toNode(term: Option[Term]): Node =
      term.map(Term.toNode).getOrElse(NodeFactory.createVariable(UUID.randomUUID().toString()))
    tpfEndpoint.handle { case (dataset, version, s, p, o, g, page, values, requestQuery) =>
      val filteredS = s.filterNot(_.isBlank)
      val filteredP = p.filterNot(_.isBlank)
      val filteredO = o.filterNot(_.isBlank)
      val filteredG = g.filterNot(_.isBlank)
      val quadPattern = Quad.create(toNode(filteredG), toNode(filteredS), toNode(filteredP), toNode(filteredO))
      if dataset != Service.DatasetId then Left(Problem.notFound(s"there is no dataset `$dataset` here; this server serves `s2`"))
      else if version != Service.Version then
        Left(Problem.notFound(s"there is no release `$version`; this server serves ${Service.Version}"))
      else
        values
          .map(S2Graph.quads(quadPattern, _))
          .getOrElse(S2Graph.quads(quadPattern).map(Matches.of))
          .left
          .map(Problem.refused)
          .map(matches =>
            QuadPatternFragment.qpf(
              matches,
              Parameters(filteredS, filteredP, filteredO, filteredG, page, values),
              Map(
                "kwg" -> S2Graph.KWGResource,
                "" -> S2Graph.KWGOnt,
                "geo" -> S2Graph.GeoSPARQL,
                "spatial-full" -> S2Graph.SpatialFull
              ),
              tpfService,
              requestQuery
            )
          )
    }

  /** Every route this server answers, in matching order: the KGF operations, the TPF route, and the catch-all last */
  val all: List[ServerEndpoint[Any, Identity]] = kgf.Endpoints.api ++ List(tpfServerEndpoint, kgf.Endpoints.unmatched)
