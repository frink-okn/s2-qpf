package org.renci.frink.kgf

import sttp.model.StatusCode

/** The error codes of KGF doc 03 section 3.6.1. The set is closed: an agent can only correct against a vocabulary it knows in advance, so a
  * condition this server meets is reported with one of these names and status, or not at all.
  */
enum Code(val status: StatusCode, val title: String):
  case BadTermSyntax extends Code(StatusCode.BadRequest, "Bad Request")
  case MalformedRequest extends Code(StatusCode.BadRequest, "Bad Request")
  case CapExceeded extends Code(StatusCode.BadRequest, "Bad Request")
  case StaleCursor extends Code(StatusCode.BadRequest, "Bad Request")
  case UnsupportedFormat extends Code(StatusCode.BadRequest, "Bad Request")
  case NotFound extends Code(StatusCode.NotFound, "Not Found")
  case MethodNotAllowed extends Code(StatusCode.MethodNotAllowed, "Method Not Allowed")
  case NotAcceptable extends Code(StatusCode.NotAcceptable, "Not Acceptable")
  case PayloadTooLarge extends Code(StatusCode.PayloadTooLarge, "Content Too Large")
  case UnsupportedMediaType extends Code(StatusCode.UnsupportedMediaType, "Unsupported Media Type")
  case InternalError extends Code(StatusCode.InternalServerError, "Internal Server Error")
  case CapabilityNotAvailable extends Code(StatusCode.NotImplemented, "Not Implemented")
  case PatternNotSupported extends Code(StatusCode.NotImplemented, "Not Implemented")

  /** The spelling that goes on the wire */
  def name: String = this match
    case BadTermSyntax          => "bad_term_syntax"
    case MalformedRequest       => "malformed_request"
    case CapExceeded            => "cap_exceeded"
    case StaleCursor            => "stale_cursor"
    case UnsupportedFormat      => "unsupported_format"
    case NotFound               => "not_found"
    case MethodNotAllowed       => "method_not_allowed"
    case NotAcceptable          => "not_acceptable"
    case PayloadTooLarge        => "payload_too_large"
    case UnsupportedMediaType   => "unsupported_media_type"
    case InternalError          => "internal_error"
    case CapabilityNotAvailable => "capability_not_available"
    case PatternNotSupported    => "pattern_not_supported"

/** An RFC 9457 problem. `type` is `about:blank` and `title` the status reason phrase, as KGF doc 03 section 3.6.1 requires: the semantics
  * live in `code` and `detail`, which is what a client is told to read.
  */
final case class Problem(code: Code, detail: String, instance: String = "", headers: Seq[(String, String)] = Seq.empty):

  def at(path: String): Problem = copy(instance = path)

  def toJson: Json =
    Json.Obj(
      Vector(
        "type" -> Json.Str("about:blank"),
        "title" -> Json.Str(code.title),
        "status" -> Json.int(code.status.code),
        "detail" -> Json.Str(detail)
      ) ++ Option.when(instance.nonEmpty)("instance" -> Json.Str(instance)) ++ Vector("code" -> Json.Str(code.name))
    )

  def body: String = Json.write(toJson)

object Problem:
  val MediaType = "application/problem+json"

  def notFound(detail: String): Problem = Problem(Code.NotFound, detail)

  def malformed(detail: String): Problem = Problem(Code.MalformedRequest, detail)

  def badTerm(detail: String): Problem = Problem(Code.BadTermSyntax, detail)

  def capExceeded(detail: String): Problem = Problem(Code.CapExceeded, detail)

  def refused(detail: String): Problem = Problem(Code.PatternNotSupported, detail)

  def unavailable(detail: String): Problem = Problem(Code.CapabilityNotAvailable, detail)
