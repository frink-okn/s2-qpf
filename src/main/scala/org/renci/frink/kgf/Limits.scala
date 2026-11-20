package org.renci.frink.kgf

/** The caps and budgets this server publishes in its service descriptor and enforces (KGF doc 03 section 3.5). A request over a cap is
  * `cap_exceeded`; a budget reached is never an error, but a `complete: false` response with a `truncation_reason` and a cursor.
  *
  * Nothing here depends on the rest of the server, so the S2 graph can read `CandidateBudget` to decide how deep an area lookup may scan.
  */
object Limits:

  /** Rows a page may hold */
  val MaxLimit = 10000

  /** Rows a page holds when the request asks for no limit. The QPF route's page size, unchanged. */
  val DefaultLimit = 100

  /** Rows a bindings body may hold */
  val MaxBindings = 1000

  /** Child items one `/schema` collection may hold */
  val MaxSchemaItems = 1000

  /** Candidates a filtered operation may examine, independent of the result limit. Here the only filtered operation is finding cells from
    * an exact area, which scans every cell at that area's level, so this is what sets the level that lookup is refused below.
    */
  val CandidateBudget = 1000000

  val MaxOutputRows = 100000
  val MaxOutputTerms = 1000000
  val MaxResponseBytes = 64L * 1024 * 1024
  val MaxRequestBytes = 1024L * 1024
  val MaxTermBytes = 64 * 1024
  val TimeBudgetMillis = 2000

  def caps: Json = Json.obj(
    "max_limit" -> Json.int(MaxLimit),
    "default_limit" -> Json.int(DefaultLimit),
    "max_bindings" -> Json.int(MaxBindings),
    "max_schema_items" -> Json.int(MaxSchemaItems)
  )

  def budgets: Json = Json.obj(
    "max_output_rows" -> Json.int(MaxOutputRows),
    "max_output_terms" -> Json.int(MaxOutputTerms),
    "max_response_bytes" -> Json.Num(BigInt(MaxResponseBytes)),
    "max_request_bytes" -> Json.Num(BigInt(MaxRequestBytes)),
    "max_term_bytes" -> Json.int(MaxTermBytes),
    "candidate_budget" -> Json.int(CandidateBudget),
    "time_budget_ms" -> Json.int(TimeBudgetMillis)
  )
