package org.renci.frink.kgf

import org.apache.jena.graph.Node
import org.renci.frink.kgf.Operations.Page
import org.renci.frink.kgf.Operations.PatternRequest
import org.renci.frink.kgf.Schema as Partitions
import sttp.model.Header
import sttp.model.MediaType
import sttp.model.Method
import sttp.model.StatusCode
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.CodecFormat.TextHtml
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint

/** The KGF HTTP surface (KGF doc 03 section 3.2), over `Operations`, `Descriptors`, `Schema`, `Void` and `Inventory`.
  *
  * Parameters are read from the request rather than declared one by one, because an operation here takes up to a dozen of them and the
  * negotiation (`format=` against `Accept`), the caps and the cursor are shared by all of them.
  */
object Endpoints:

  final case class ApplicationJson() extends CodecFormat:
    override val mediaType: MediaType = MediaType("application", "json", Some("utf-8"))

  final case class ApplicationProblemJson() extends CodecFormat:
    override val mediaType: MediaType = MediaType("application", "problem+json", Some("utf-8"))

  final case class ApplicationNQuads() extends CodecFormat:
    override val mediaType: MediaType = MediaType("application", "n-quads", Some("utf-8"))

  final case class ApplicationTriG() extends CodecFormat:
    override val mediaType: MediaType = MediaType("application", "trig", Some("utf-8"))

  final case class TextTurtle() extends CodecFormat:
    override val mediaType: MediaType = MediaType("text", "turtle", Some("utf-8"))

  final case class ApplicationJsonLd() extends CodecFormat:
    override val mediaType: MediaType = MediaType("application", "ld+json", Some("utf-8"))

  final case class TextMarkdown() extends CodecFormat:
    override val mediaType: MediaType = MediaType("text", "markdown", Some("utf-8"))

  final case class Payload(format: Format, body: String)

  private def variant[CF <: CodecFormat](format: Format, codec: Codec[String, String, CF]) =
    oneOfVariantValueMatcher(stringBodyUtf8AnyFormat(codec).map(Payload(format, _))(_.body)) { case Payload(chosen, _) =>
      chosen == format
    }

  private val payload: EndpointOutput[Payload] = oneOf[Payload](
    variant(Format.Json, Codec.string.format(ApplicationJson())),
    variant(Format.NQuads, Codec.string.format(ApplicationNQuads())),
    variant(Format.TriG, Codec.string.format(ApplicationTriG())),
    variant(Format.Turtle, Codec.string.format(TextTurtle())),
    variant(Format.JsonLd, Codec.string.format(ApplicationJsonLd())),
    variant(Format.Markdown, Codec.string.format(TextMarkdown())),
    variant(Format.Html, Codec.string.format(TextHtml()))
  )

  /** RFC 9457 problems, whatever the client asked to accept: the semantics live in `code` and `detail`, which every client reads the same
    * way (KGF doc 03 section 3.6.1)
    */
  val problemOut: EndpointOutput[Problem] =
    statusCode
      .and(header[Option[String]]("Allow"))
      .and(stringBodyUtf8AnyFormat(Codec.string.format(ApplicationProblemJson())))
      .map[Problem] { case (status, _, body) => Problem(Code.InternalError, body) } { raised =>
        (raised.code.status, raised.headers.toMap.get("Allow"), raised.body)
      }

  private type Answer = Either[Problem, (List[Header], Payload)]

  private val Query = Method.unsafeApply("QUERY")

  private def base = endpoint.errorOut(problemOut).out(headers).out(payload)

  private def operation(name: String) =
    base.get.in(path[String]("dataset") / "v" / path[String]("version") / name).in(extractFromRequest(identity))

  private def body(name: String, method: Method) =
    base
      .method(method)
      .in(path[String]("dataset") / "v" / path[String]("version") / name)
      .in(stringBody)
      .in(extractFromRequest(identity))

  // ---------------------------------------------------------------- responding

  private def respond(
      request: ServerRequest,
      offered: Seq[Format],
      name: String,
      canonical: String,
      completeness: Completeness = Completeness.Complete,
      caching: String = Headers.Immutable
  )(render: Format => Either[Problem, String]): Answer =
    for
      format <- Format.negotiate(offered, request.queryParameters.get("format"), request.header("Accept"))
      rendered <- render(format)
    yield (Headers(format, s"$name\n$canonical", completeness, caching).map(Header(_, _)).toList, Payload(format, rendered))

  /** A JSON answer and the HTML view of the same answer */
  private def document(
      request: ServerRequest,
      name: String,
      heading: String,
      canonical: String,
      completeness: Completeness = Completeness.Complete
  )(answer: => Json): Answer =
    respond(request, Format.Tabular, name, canonical, completeness) {
      case Format.Html => Right(Html.page(heading, Html.render(answer)))
      case _           => Right(Json.write(answer))
    }

  private def parameter(request: ServerRequest, name: String): Option[String] =
    request.queryParameters.get(name).map(_.trim).filter(_.nonEmpty)

  private def limitOf(request: ServerRequest, cap: Int = Limits.MaxLimit): Either[Problem, Int] =
    parameter(request, "limit") match
      case None => Right(Limits.DefaultLimit)
      case Some(value) =>
        value.toIntOption match
          case Some(limit) if limit >= 0 && limit <= cap => Right(limit)
          case Some(limit) if limit > cap                => Left(Problem.capExceeded(s"limit=$limit is over this operation's cap of $cap"))
          case _                                         => Left(Problem.malformed(s"limit=$value is not a number of rows"))

  private def positionOf(cursor: Option[String], name: String, canonical: String): Either[Problem, BigInt] =
    cursor.map(_.trim).filter(_.nonEmpty) match
      case None        => Right(BigInt(0))
      case Some(token) => Cursor.decode(token, name, canonical)

  private def completenessOf(name: String, canonical: String, page: Page): Completeness =
    if page.complete then Completeness.Complete
    else Completeness.pageLimit(Cursor.encode(name, canonical, page.nextPosition))

  /** A capability this server does not declare, named in the request (KGF doc 03 section 3.1 principle 5) */
  private def undeclared(request: ServerRequest): Option[Problem] =
    val asked = Vector(
      "o.text" -> "search",
      "o.ge" -> "range",
      "o.gt" -> "range",
      "o.le" -> "range",
      "o.lt" -> "range",
      "labels" -> "labels"
    ).collectFirst { case (name, capability) if parameter(request, name).isDefined => name -> capability }
    asked.map((name, capability) =>
      Problem.unavailable(
        s"$name needs the `$capability` capability, which this dataset does not declare; it declares " +
          s"${Service.Capabilities.mkString(" and ")} beside the core profile"
      )
    )

  // ---------------------------------------------------------------- operations

  private def fragmentAnswer(
      request: ServerRequest,
      pattern: PatternRequest,
      rows: Seq[Map[String, Node]],
      limit: Int,
      cursor: Option[String]
  ): Answer =
    val canonical = s"${pattern.canonical}\n${rows.size}"
    for
      from <- positionOf(cursor, "fragment", canonical)
      page <-
        if rows.isEmpty then Operations.quads(pattern).map(Operations.page(_, from, limit))
        else
          Operations
            .restricted(pattern, rows)
            .map(found =>
              Page(found.drop(from).iterator.take(limit).map((index, quad) => (Some(index), quad)).toSeq, found.size, from, limit)
            )
      completeness = completenessOf("fragment", canonical, page)
      answer <- respond(request, Format.Data, "fragment", canonical, completeness) {
        case Format.Json => Right(Json.write(Envelopes.fragment(pattern, page, completeness)))
        case Format.Html => Right(Html.page("fragment", Html.render(Envelopes.fragment(pattern, page, completeness))))
        case rdf         => Rdf.quads(page.rows.map(_._2), rdf, pattern.scope.quadView)
      }
    yield answer

  private def countAnswer(request: ServerRequest, pattern: PatternRequest, rows: Seq[Map[String, Node]]): Answer =
    val canonical = s"${pattern.canonical}\n${rows.size}"
    if rows.isEmpty then
      Operations
        .quads(pattern)
        .flatMap(found => document(request, "count", "count", canonical)(Envelopes.count(pattern, found.size)))
    else
      Operations
        .counts(pattern, rows)
        .flatMap(counts => document(request, "count", "count", canonical)(Envelopes.counts(pattern, counts)))

  private def patternOf(request: ServerRequest): Either[Problem, PatternRequest] =
    PatternRequest.parse(
      parameter(request, "s"),
      parameter(request, "p"),
      parameter(request, "o"),
      parameter(request, "g"),
      parameter(request, "o.dt"),
      parameter(request, "o.lang")
    )

  // ---------------------------------------------------------------- endpoints

  private def segments(request: ServerRequest): List[String] = request.pathSegments.filter(_.nonEmpty)

  private def versioned(dataset: String, version: String, request: ServerRequest): Either[Problem, Unit] =
    if segments(request).size != 4 then Left(Problem.notFound(s"no such resource: /${segments(request).mkString("/")}"))
    else if dataset != Service.DatasetId then
      Left(Problem.notFound(s"there is no dataset `$dataset` here; this server serves `${Service.DatasetId}`"))
    else if version != Service.Version then
      Left(
        Problem.notFound(
          s"there is no release `$version` of `$dataset`; a computed graph serves the release it is, which is ${Service.Version}"
        )
      )
    else Right(())

  private val health: ServerEndpoint[Any, Identity] =
    endpoint.get.in("healthz").out(stringBody).handleSuccess(_ => "ok\n")

  private val datasetDescriptor: ServerEndpoint[Any, Identity] =
    base.get
      .in(path[String]("dataset"))
      .in(extractFromRequest(identity))
      .handle { (dataset, request) =>
        if segments(request).size != 1 then Left(Problem.notFound(s"no such resource: /${segments(request).mkString("/")}"))
        else if dataset != Service.DatasetId then
          Left(Problem.notFound(s"there is no dataset `$dataset` here; this server serves `${Service.DatasetId}`"))
        else document(request, "dataset", "dataset", "")(Descriptors.dataset)
      }

  /** Method-preserving, so that a body-carrying QUERY or POST is not rewritten to GET (KGF doc 03 section 3.2) */
  private val latest: ServerEndpoint[Any, Identity] =
    endpoint
      .in(path[String]("dataset") / "latest" / paths)
      .in(extractFromRequest(identity))
      .errorOut(problemOut)
      .out(statusCode(StatusCode.TemporaryRedirect))
      .out(header[String]("Location"))
      .out(header[String]("Cache-Control"))
      .handle { (dataset, rest, request) =>
        if dataset != Service.DatasetId then Left(Problem.notFound(s"there is no dataset `$dataset` here"))
        else
          val query = request.queryParameters.toString match
            case ""    => ""
            case other => s"?$other"
          Right((Service.link(s"${Service.VersionPath}/${rest.mkString("/")}$query"), Headers.Redirect))
      }

  private val manifest: ServerEndpoint[Any, Identity] =
    operation("manifest").handle { (dataset, version, request) =>
      versioned(dataset, version, request).flatMap(_ => document(request, "manifest", "manifest", "")(Descriptors.manifest))
    }

  private val fragment: ServerEndpoint[Any, Identity] =
    operation("fragment").handle { (dataset, version, request) =>
      for
        _ <- versioned(dataset, version, request)
        _ <- undeclared(request).toLeft(())
        pattern <- patternOf(request)
        limit <- limitOf(request)
        answer <- fragmentAnswer(request, pattern, Seq.empty, limit, parameter(request, "cursor"))
      yield answer
    }

  private def bindingFragment(method: Method): ServerEndpoint[Any, Identity] =
    body("fragment", method).handle { (dataset, version, text, request) =>
      for
        _ <- versioned(dataset, version, request)
        _ <- undeclared(request).toLeft(())
        parsed <- Bodies.parse(text, Set("pattern", "bindings", "g", "limit", "cursor"))
        limit <- parsed.limit match
          case None => Right(Limits.DefaultLimit)
          case Some(value) =>
            if value < 0 || value > Limits.MaxLimit then Left(Problem.capExceeded(s"limit=$value is over the cap of ${Limits.MaxLimit}"))
            else Right(value)
        answer <- fragmentAnswer(request, parsed.pattern, parsed.rows, limit, parsed.cursor)
      yield answer
    }

  private val count: ServerEndpoint[Any, Identity] =
    operation("count").handle { (dataset, version, request) =>
      for
        _ <- versioned(dataset, version, request)
        _ <- undeclared(request).toLeft(())
        pattern <- patternOf(request)
        answer <- countAnswer(request, pattern, Seq.empty)
      yield answer
    }

  private def bindingCount(method: Method): ServerEndpoint[Any, Identity] =
    body("count", method).handle { (dataset, version, text, request) =>
      for
        _ <- versioned(dataset, version, request)
        parsed <- Bodies.parse(text, Set("pattern", "bindings", "g"))
        answer <- countAnswer(request, parsed.pattern, parsed.rows)
      yield answer
    }

  private val describe: ServerEndpoint[Any, Identity] =
    operation("describe").handle { (dataset, version, request) =>
      for
        _ <- versioned(dataset, version, request)
        _ <- undeclared(request).toLeft(())
        iri <- parameter(request, "iri").toRight(Problem.malformed("describe takes an `iri` to describe")).flatMap(Term.parse)
        _ <- Either.cond(iri.isURI(), (), Problem.malformed("`iri` names a resource with an IRI in angle brackets"))
        direction = parameter(request, "direction").getOrElse("both")
        scope <- Operations.Scope.parse(parameter(request, "g"))
        limit <- limitOf(request)
        canonical = s"${Term.encode(iri)}\n$direction\n${scope.raw.getOrElse("")}"
        from <- positionOf(parameter(request, "cursor"), "describe", canonical)
        found <- Operations.describe(iri, direction, scope)
        page = Operations.page(found, from, limit)
        completeness = completenessOf("describe", canonical, page)
        answer <- respond(request, Format.Tabular, "describe", canonical, completeness) {
          case Format.Html =>
            Right(Html.page("describe", Html.render(Envelopes.describe(iri, direction, page, completeness))))
          case _ => Right(Json.write(Envelopes.describe(iri, direction, page, completeness)))
        }
      yield answer
    }

  private val graphs: ServerEndpoint[Any, Identity] =
    operation("graphs").handle { (dataset, version, request) =>
      for
        _ <- versioned(dataset, version, request)
        limit <- limitOf(request)
        from <- positionOf(parameter(request, "cursor"), "graphs", "")
        found = Operations.graphs
        rows = found.drop(from).iterator.take(limit).toSeq
        complete = from + rows.size >= found.size
        completeness =
          if complete then Completeness.Complete else Completeness.pageLimit(Cursor.encode("graphs", "", from + rows.size))
        answer <- document(request, "graphs", "graphs", "", completeness)(Envelopes.graphs(rows, found.size, completeness))
      yield answer
    }

  private val terms: ServerEndpoint[Any, Identity] =
    operation("terms").handle { (dataset, version, request) =>
      for
        _ <- versioned(dataset, version, request)
        prefix <- parameter(request, "prefix").toRight(
          Problem.malformed("terms takes a `prefix` to scan, written bare rather than as a term")
        )
        role <- Inventory.Role.parse(parameter(request, "role").getOrElse("any"))
        counting = parameter(request, "count").exists(value => value == "true" || value == "1")
        limit <- limitOf(request)
        canonical = s"$prefix\n${role.name}"
        answer <-
          if counting then document(request, "terms", "terms", canonical)(Envelopes.termCount(prefix, role, Inventory.counts(prefix)))
          else
            for
              from <- positionOf(parameter(request, "cursor"), "terms", canonical)
              found = Inventory.terms(prefix, role)
              rows = found.drop(from).iterator.take(limit).toSeq
              complete = from + rows.size >= found.size
              completeness =
                if complete then Completeness.Complete
                else Completeness.pageLimit(Cursor.encode("terms", canonical, from + rows.size))
              page <- document(request, "terms", "terms", canonical, completeness)(
                Envelopes.terms(prefix, role, rows, found.size, completeness)
              )
            yield page
      yield answer
    }

  private val schema: ServerEndpoint[Any, Identity] =
    operation("schema").handle { (dataset, version, request) =>
      for
        _ <- versioned(dataset, version, request)
        _ <- undeclared(request).toLeft(())
        view <- Model
          .view(parameter(request, "view").getOrElse("design"))
          .toRight(
            Problem.notFound(
              s"there is no view by that name; this dataset has design, queryable, and one per level graph, which `graphs` lists"
            )
          )
        classTerm <- parameter(request, "class").map(Term.parse(_).map(Some(_))).getOrElse(Right(None))
        predicate <- parameter(request, "predicate").map(Term.parse(_).map(Some(_))).getOrElse(Right(None))
        datatype <- parameter(request, "datatype").map(Term.parse(_).map(Some(_))).getOrElse(Right(None))
        limit <- limitOf(request, Limits.MaxSchemaItems)
        requested = Partitions.Request(
          view,
          classTerm,
          predicate,
          datatype,
          parameter(request, "children"),
          parameter(request, "projection"),
          limit,
          parameter(request, "cursor")
        )
        answered <- Partitions.answer(requested)
        answer <- respond(request, Format.Tabular, "schema", requested.canonical) {
          case Format.Html => Right(Html.page("schema", Html.render(answered)))
          case _           => Right(Json.write(answered))
        }
      yield answer
    }

  private val void: ServerEndpoint[Any, Identity] =
    operation("void").handle { (dataset, version, request) =>
      for
        _ <- versioned(dataset, version, request)
        answer <- respond(request, Format.Rdf, "void", "") {
          case Format.Html => Void.document(Format.Turtle).map(text => Html.document("void", text))
          case rdf         => Void.document(rdf)
        }
      yield answer
    }

  private val summary: ServerEndpoint[Any, Identity] =
    operation("summary").handle { (dataset, version, request) =>
      for
        _ <- versioned(dataset, version, request)
        answer <- respond(request, Format.Card, "summary", "") {
          case Format.Markdown => Right(Descriptors.summaryMarkdown)
          case Format.Html     => Right(Html.document("summary", Descriptors.summaryMarkdown))
          case _               => Right(Json.write(Descriptors.summaryJson))
        }
      yield answer
    }

  /** Anything the routes above do not match. A path that is an operation under another method answers 405 with `Allow`, which is the one
    * thing a status alone leaves ambiguous for a client asking whether a resource takes QUERY (KGF doc 03 section 3.6.1).
    */
  val unmatched: ServerEndpoint[Any, Identity] =
    endpoint
      .in(paths)
      .in(extractFromRequest(identity))
      .errorOut(problemOut)
      .out(headers)
      .out(payload)
      .handle { (path, request) =>
        val last = path.filter(_.nonEmpty).lastOption.getOrElse("")
        if path.forall(_.isEmpty) then
          if request.method == Method.GET || request.method == Method.HEAD then
            document(request, "service", "service", "")(Descriptors.service)
          else Left(Problem(Code.MethodNotAllowed, "the service descriptor takes GET", headers = Vector("Allow" -> "GET, HEAD")))
        else if Vector("fragment", "count").contains(last) then
          Left(
            Problem(Code.MethodNotAllowed, s"$last takes GET, QUERY and POST", headers = Vector("Allow" -> "GET, QUERY, POST, HEAD"))
          )
        else if Service.Operations.contains(last) then
          Left(Problem(Code.MethodNotAllowed, s"$last takes GET", headers = Vector("Allow" -> "GET, HEAD")))
        else
          Left(
            Problem.notFound(
              s"no such resource: /${path.mkString("/")}. The service descriptor at ${Service.link("/")} links every operation"
            )
          )
      }

  /** The KGF operations, in matching order: the fixed paths before the dataset wildcard. The catch-all is added last, after the TPF route,
    * by `org.renci.frink.Endpoints.all`.
    */
  val api: List[ServerEndpoint[Any, Identity]] = List(
    health,
    manifest,
    bindingFragment(Query),
    bindingFragment(Method.POST),
    fragment,
    bindingCount(Query),
    bindingCount(Method.POST),
    count,
    describe,
    graphs,
    terms,
    schema,
    void,
    summary,
    latest,
    datasetDescriptor
  )
