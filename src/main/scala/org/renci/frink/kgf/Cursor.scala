package org.renci.frink.kgf

import java.security.MessageDigest
import java.util.Base64
import scala.util.Try

/** An opaque resume token carrying a position in an enumeration (KGF doc 03 section 3.6).
  *
  * This server's enumeration order is fixed and `SizedIterator.drop` skips positions arithmetically, so a position is all a cursor needs to
  * hold and a page at offset 10^18 costs what page 1 costs. The token binds the content digest, the operation and the canonical request;
  * presented against any other it is `stale_cursor`. Nothing in it is secret: it is a position plus a fingerprint of what it is a position
  * into, so a forged token is rejected for not matching rather than for failing a signature.
  */
object Cursor:

  def encode(operation: String, request: String, position: BigInt): String =
    val token = s"${fingerprint(operation, request)}.$position"
    Base64.getUrlEncoder.withoutPadding.encodeToString(token.getBytes("UTF-8"))

  def decode(token: String, operation: String, request: String): Either[Problem, BigInt] =
    val stale = Problem(
      Code.StaleCursor,
      "this cursor is for a different version, operation or request; start the enumeration again from its first page"
    )
    Try(String(Base64.getUrlDecoder.decode(token), "UTF-8")).toOption
      .map(_.split('.'))
      .collect { case Array(digest, position) => digest -> position }
      .flatMap { case (digest, position) => Try(BigInt(position)).toOption.map(digest -> _) }
      .filter { case (digest, position) => digest == fingerprint(operation, request) && position >= 0 }
      .map { case (_, position) => Right(position) }
      .getOrElse(Left(stale))

  private def fingerprint(operation: String, request: String): String =
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(s"${Service.ContentDigest}\n$operation\n$request".getBytes("UTF-8"))
    digest.digest().take(8).map(byte => f"${byte & 0xff}%02x").mkString
