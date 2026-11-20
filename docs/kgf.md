# The KGF surface: what this server serves, and how it honors the contract

Decided 2026-11-19. This server is a **computed graph** (KGF doc 05 §5.8): it answers
from the `s2-geometry` library rather than from a stored bundle. It speaks
[KGF](https://github.com/frink-okn/kgf) because it can honor the cost contract, not
because it holds a bundle. `../kgf` doc 03 is normative for the HTTP API; where this
doc differs from it, the difference is stated here and in the same words as the reason,
and the divergence belongs in one of the two documents rather than only in code.

This replaces the `/qpf` route. The QPF/brTPF hypermedia form continues on
`/{dataset}/v/{version}/tpf` as doc 03 §3.8's compatibility layer; no old route is
kept as an alias.

## 1. Identity

| | |
|---|---|
| dataset id | `s2` — the OKN registry shortname (`okn-registry/docs/registry/kgs/s2.md`) |
| dataset IRI | `https://purl.org/okn/frink/kg/s2` |
| version | the release in `project.scala`, prefixed: `publish.version` `1.2.0` → `v1.2.0` |
| content digest | `sha256:…` over the code artifact the process is running, computed at startup |
| class | `computed`: no bundle, no tier-0 custody promise (doc 05 §5.8, doc 07 §7.5 item 34(d)) |

**Why the version is the release and the digest is the code.** A bundle-backed dataset
versions its data; here the data is the program, so a release of the program is a
release of the dataset. Every answer is a function of the generator and the S2 library
it links, so two deployments of one release answer identically, which is what a
`content_digest` has to mean for cursors, `ETag`s and receipts to work (doc 05 §5.8).
The digest is computed by hashing the jar entries the server classes load from, so a
development run has one without configuration; `KGF_CONTENT_DIGEST` overrides it where
the deployment knows the container image digest, which is the artifact doc 05 names.

**Service base.** `KGF_PUBLIC_BASE` (default `http://localhost:8080`) is the base every
emitted link carries, and the server's own router is mounted at `/` and strips nothing
(doc 03 §3.2). In production the gateway routes `/s2` to this pod and removes that
prefix, so the base is `https://frink.apps.renci.org/s2` and the dataset descriptor is
`https://frink.apps.renci.org/s2/s2`. The doubled segment is cosmetic: it keeps the
whole documented URL space — the service descriptor at the base, `/healthz` beside it —
reachable, which mounting the dataset at the gateway prefix would not.

`QPF_SERVER_LOCATION` is gone. `HTTP_PORT` is unchanged.

## 2. The URL space

```
/                                 service descriptor: this dataset, caps, budgets
/healthz                          200 if the process can answer; reads nothing
/s2                               dataset descriptor: identity, releases, links
/s2/latest/{*rest}                307 to the current version, method-preserving
/s2/v/{version}/manifest          release identity, capabilities, prefixes, counts
/s2/v/{version}/fragment          GET pattern; QUERY/POST pattern + bindings
/s2/v/{version}/count             GET pattern; QUERY/POST pattern + bindings
/s2/v/{version}/describe          one IRI's neighborhood
/s2/v/{version}/graphs            the 31 level graphs with exact counts
/s2/v/{version}/terms             dictionary prefix scan and closed-form counts
/s2/v/{version}/void              VoID, serialized per request
/s2/v/{version}/schema            the VoID partition tree, paged
/s2/v/{version}/summary           the summary card, Markdown or JSON
/s2/v/{version}/tpf               QPF/brTPF compatibility (doc 03 §3.8)
```

A version that is not this release's is `404 not_found`: a computed graph serves one
release, the one it is.

## 3. Capabilities

Declared: **`graphs`** and **`terms`**, beside the mandatory core profile (`fragment`
GET and QUERY-with-bindings, `count`, `describe`, `manifest`, `void`, `schema`,
`summary` — doc 03 §3.1 principle 5).

Not declared, and `501 capability_not_available` when a request needs one:

| capability | why not |
|---|---|
| `export`, `filters`, `keysets` | each needs the term set enumerated (doc 05 §5.8); 10¹⁹ cells cannot be |
| `search` | no text index, and `o.text`'s ranked window has no closed form here |
| `range` | a candidate for areas and cell ids later; nothing is promised now |
| `sample`, `distinct`, `labels`, `star`, `closure`, `vectors`, `reification`, `geo` | not in this pass. `sample` and `labels` are cheap here and `geo` is the natural one to add (an S2 covering answers `within=`/`near=` directly), so each is a later decision, not a refusal in principle |

A capability is all-or-nothing: declaring one commits to its whole contract — methods,
formats and response metadata (doc 03 §3.1) — so none is half-served.

## 4. Representations

`format=` or `Accept`, per operation, as `kgf-rs` offers them:

| operation | formats |
|---|---|
| `fragment` | `json` (default), `nq`, `trig`, `ttl`, `jsonld`, `html` |
| `count`, `describe`, `graphs`, `terms`, `schema`, `manifest`, and the descriptors | `json`, `html` |
| `void` | `ttl`, `jsonld`, `html` |
| `summary` | `md`, `json`, `html` |
| `tpf` | `trig`, `nq`, `jsonld`, `html` |

Anything else is `400 unsupported_format` naming what that operation offers. `describe`
returns quads and could serve the RDF syntaxes; it offers `json` and `html` because that
is what `kgf-rs` offers, and one story across KGF servers is worth more here than the
extra serialization. Doc 03 section 3.4.6 reads as though it serves triples, which is a
question for `../kgf` rather than a divergence to invent alone.

This matches `kgf-rs`, and **diverges from doc 03 §3.1 principle 3**, which makes
SPARQL Results JSON, CSV, Parquet and Arrow peer formats of every bindings-shaped
operation. No KGF server offers them today. The divergence is recorded rather than
quietly implemented: a client that asks for `csv` gets the coded error that says so.

`ttl` is single-graph, so it serves the union and a scoped view and refuses the quad
view with `406 not_acceptable`.

Caching, as doc 03 §3.6: versioned GETs carry `Cache-Control: public, max-age=31536000,
immutable`; `latest` redirects carry `max-age=300`; `ETag` is derived from the content
digest, the canonical request and the representation, with `Vary: Accept`.

Completeness rides both the JSON envelope (`complete`, `truncation_reason`, `next`) and
the headers (`KGF-Complete`, `KGF-Truncation-Reason`, `KGF-Next-Cursor`), so the RDF and
HTML representations carry it too.

## 5. Counts

Every count this server reports is **exact and computed in closed form**, and is written
as a JSON number in full digits however large (doc 03 §3.4.4). `Util.SizedIterator`
already gives an exact `BigInt` size for every pattern, so `cardinality` and `/count`
are the same number from the same code that pages the rows — there is no second
counting path to drift from the first. The dataset's own total is
1,361,984,604,108,888,559,446, which passes 2⁶⁴; `BigInt` throughout is what makes that
unremarkable.

## 6. Patterns, scoping, and refusal

`s`, `p`, `o` and `g` take doc 03 §3.3 syntax: an IRI **in angle brackets**, a CURIE
against the manifest prefix map, a quoted literal with optional `@lang` or `^^dt`, an
omitted parameter for a variable. This is a real change from the QPF route, where
Hydra's explicit representation writes IRIs bare; both forms stay, each on its own
route, because each is its route's standard.

`o.lang` and `o.dt` are answered exactly, not by scanning: each predicate here has one
object datatype (`rdfs:label` → `xsd:string`, `kwg-ont:cellID` → `xsd:integer`,
`geo:hasMetricArea` → `xsd:double`, `geo:asWKT` → `geo:wktLiteral`, every other object
an IRI), so a datatype restriction is a predicate restriction. No literal here carries a
language tag, so `o.lang` matches nothing, exactly.

**One declared vocabulary.** The prefix map the manifest publishes is the only one this
server declares: a request's CURIEs resolve against it, every RDF representation of every
route declares it, and every HTML page abbreviates with it. Its names and namespaces are
the OKN registry's shared prefix table, `okn-registry/docs/registry/prefixes.yaml`, which
`ServiceSpec` checks them against where that checkout sits beside this one. Only a
document's own hypermedia adds to it — the TPF document declares `hydra`, `dct`, `foaf`
and `sd` for its controls, and the VoID document `dct`, `sd` and `foaf` for its own.
`hydra` is the one name not in the shared table, which is a table of the vocabularies the
federation's graphs publish rather than of anyone's controls. Until 2026-11-20 the TPF
route declared a map of its own, with the empty prefix for the KWG ontology, so one term
read two ways depending on the route that showed it.

Blank nodes are parsed and match nothing (doc 03 §3.3). No data blank node exists here,
so doc 03 §3.4.1's skolem URN never appears.

**A term in two positions matches nothing.** No quad of this graph has one term in two
positions: nothing is related to itself, and graphs, predicates and classes are never
cells.

**Graph scoping** follows [`graph-semantics.md`](graph-semantics.md), the contract shared
with KGF, which that document states in full. In KGF's parameter form:

| `g` | Selection | Count |
|---|---|---|
| absent, or `<urn:x-kgf:union>` | every level, one row per triple | the union's triples |
| `<kwg:s2.level{N}>` | that level | that level's triples |
| `<urn:x-kgf:unnamed>` | empty | 0 |
| any other IRI | empty | 0 |
| `*` | the quad view: every row with a `g` column holding its level graph | memberships |

Every quad here is in exactly one level graph, so the union and the quad view have the
same cardinality, and Σ per-level counts equals both. `urn:ldf:defaultGraph` stays an
accepted alias for the union, as the name this server advertised before KGF's.

**Refusal.** `501 pattern_not_supported` for exactly one pattern family: finding cells
from a `geo:hasMetricArea` value whose level is deeper than 8, which would mean scanning
every cell at that level (doc 03 §3.4.1). The limit is now *derived* rather than timed:
level 8 is 393,216 cells, within the published `candidate_budget` of 1,000,000, and
level 9 is 1,572,864, past it. `detail` names the area, the level, and the limit. Every
other pattern is answered within the contract — nothing in between (doc 03 §3.4.1).

Refusal depends on a bound value, not on the pattern's shape, which is why it is
reported when the request arrives rather than declared in the descriptor, and why
`/summary` states it in prose.

## 7. Bindings

`QUERY /fragment` and `QUERY /count`, with `POST` as the fallback carrying identical
semantics (doc 03 §3.1 principle 2), take the JSON body:

```json
{"pattern": {"s": "?cell", "p": "<…/sfTouches>", "o": "?n"},
 "bindings": {"vars": ["?cell"], "rows": [["<…/s2.level13.5522341869704445952>"]]},
 "g": "*", "limit": 1000, "cursor": "…"}
```

Each result row carries `binding`, the index of the input row it joins with; rows
enumerate in input-row order; `limit` is **global** across all bindings; `max_bindings`
is 1,000.

**This is where the KGF path is simpler than the QPF one, and better.** On the QPF route
a response is one set of quads with one total, so a quad matching two binding rows can
only appear once — hence `qpf.Matches`, whose positions are `Option[Quad]` with `None`
where an earlier restriction already matched, short pages, and a total that is only an
upper bound (`exact: false`). With a `binding` column the answer is one row per
(input row, quad) pair, so nothing is deduplicated, the cardinality is exactly Σ of the
per-row counts, and `exact` is always true. `Matches` therefore stays on the `/tpf`
route only.

One refused row refuses the whole request, and `detail` names the refused row indices so
a client can drop them and resend (doc 03 §3.4.1).

## 8. Cursors

A cursor is an opaque token carrying a **position** — this server's enumeration order is
fixed and `SizedIterator.drop` is positional and skips without enumerating, so a page at
offset 10¹⁸ costs what page 1 costs. The token binds the content digest, the operation
and the canonical request, as doc 03 §3.6 requires; presented against any other, it is
`400 stale_cursor`. Nothing about a cursor is secret: it is a position plus a fingerprint
of what it is a position into, so a forged one is rejected for not matching rather than
for failing a signature.

`page=` is gone from the KGF routes. The `/tpf` route keeps Hydra paging, which is what
its clients follow.

## 9. Caps and budgets

Published in the service descriptor, and enforced:

| cap | value | |
|---|---|---|
| `max_limit` | 10,000 | rows per page |
| `default_limit` | 100 | the QPF page size, unchanged |
| `max_bindings` | 1,000 | rows in a bindings body |
| `max_schema_items` | 1,000 | child items in a `/schema` collection |
| `candidate_budget` | 1,000,000 | candidates a filtered operation examines — here, cells scanned for an area lookup, which is what sets the refusal level in §6 |
| `max_response_bytes` | 64 MB | |
| `max_request_bytes` | 1 MB | |
| `max_term_bytes` | 64 KB | a level-0 cell's WKT is the longest term this server emits, far below it |
| `max_output_rows` | 100,000 | |
| `max_output_terms` | 1,000,000 | |
| time budget | 2 s | soft |

A request over a cap is `400 cap_exceeded`. A budget reached is never an error: the
response is `complete: false` with a `truncation_reason` and a cursor (doc 03 §3.5).

## 10. `terms`: the namespace inventory the federation index needs

The federation index leaves a graph out of an answer only on evidence that the graph
cannot match, and a graph with no evidence is asked every time (doc 05 §5.8, doc 07 §7.5
item 39). A stored graph offers membership filters; this one cannot build any, so its
only evidence is its **namespace inventory**, which must be complete and countable for
any prefix in the federation's shared table — including one added after this release, and
one nested inside the cell prefixes, such as a prefix for a single level. That is
`GET /terms?prefix=…&count=true`, answered in closed form.

**How the closed form works.** The valid cell ids at level L are an arithmetic
progression: first `S2CellId.begin(L).id()`, step `2^(61-2L)`, count `6·4^L`. A decimal
prefix `P` selects the ids in the union of ranges `[P·10^k, P·10^k + 10^k − 1]`, so the
count is a division per range — exact, and independent of how many ids match. A prefix
that stops short of a level's id part counts that whole level, and one that reaches into
it counts a progression within a range. A level graph IRI (`…s2.level13`) is itself a
prefix of every level-13 cell IRI, so that prefix counts those cells — but never the
graph name, which fills no triple position and which `/graphs` lists instead.
Thirty-one levels, two IRI families, and a fixed vocabulary of 11 predicates and 35
classes make up everything this server holds.

**The order terms enumerate in** is families by IRI stem, then cell ids ascending. Doc 03
describes a scan over a sorted dictionary, and this server has no dictionary to sort: the
set of terms under a prefix is the same either way, numeric order is what a cell id
means, and it is what makes a cursor an arithmetic skip rather than a walk of a decimal
trie. The order is fixed, which is what a cursor needs of it.

`role` is `subject`, `predicate`, `object` or `any`. Every cell IRI and every geometry
IRI is both a subject and an object here; classes are objects only; the 11 predicates are
predicates only; a level graph IRI is a graph name and fills no triple position.

**Prefix scans enumerate IRIs only.** A prefix over literals would need this server to
publish a term order over values it computes rather than stores, and the inventory doc 05
asks for is over IRIs. A literal prefix counts 0, and `/terms` says so in its response
rather than in an error.

The prefixes this server's IRIs lie under, all in the shared table: `kwg-s2:`
(`…/lod/resource/s2.`), `kwg-s2geom:` (`…/lod/resource/geometry.polygon.s2.`),
`kwg-ont:`, `geo:`, `spatial-full:`, `rdf:`, `rdfs:`. Nothing else is emitted, so the
residual is zero in every role. **A predicate or class added to the generator is added to
the shared table in the same change**, or the inventory stops being complete and this
server is asked about every IRI in the federation.

## 11. `graphs`

`GET /graphs` lists the 31 level graphs, each with its exact triple count, in level order
— which is a contract, because it is what an outstanding cursor indexes. The unnamed
graph is never listed: every triple here carries a level. `triples` and `memberships` in
the envelope are equal for the same reason.

## 12. The description surface

`/manifest` keeps the ordinary shape (doc 05 §5.8: "the manifest keeps the ordinary
shape"), with the generator in place of `data.hdt`: identity and `content_digest` as in
§1, `counts` (closed-form triples, distinct subjects, predicates, objects), the declared
`capabilities`, the `prefixes` map, and an `artifacts` block naming the generator release
rather than a bundle's files. No checksummed file list, because there are no files — and
the manifest says that rather than omitting the block.

`/void` carries the dataset with its totals, the whole class and property tree of the
queryable view with datatype partitions, and one `void:subset` per level graph with that
graph's totals and property partitions. It stops short of the class tree of each of the
31 level graphs, which would be some twelve thousand partitions and several megabytes;
`/schema?view=graph:<IRI>` is the drill-down for that, which is what `/schema` is for
(doc 03 section 3.4.10). It states no total for distinct objects: two cells can carry the
same `geo:hasMetricArea` literal and how many do is not closed-form, so that statistic is
left out rather than published as an upper bound that looks exact — which is what doc 03
says of a statistic a partition does not state.

`/void`, `/schema` and `/summary` describe the same closed-form model:

- **views**: `queryable` (the union of all levels) and `design`, its alias here because
  the bundle declares no components (doc 03 §3.4.10), plus `graph:<level IRI>` for each
  of the 31 level graphs.
- **classes**: `kwg-ont:S2Cell`, `kwg-ont:S2Cell_Level{N}` for each level, `geo:Feature`,
  `geo:SpatialObject`, `geo:Geometry`.
- **properties** within each class partition, with `distinct_subjects` and
  `distinct_objects`, and **datatype** partitions under the four literal-valued ones.
- **class relations** for the four relation predicates. A cell is typed four ways
  (`S2Cell`, its level class, `Feature`, `SpatialObject`), so one triple contributes to
  several rows and these counts do not sum to the triple count — which doc 03 §3.4.10
  says of every dataset's class relations, and is simply visible here.

Every number in them is the size of the pattern iterator that answers the matching
`/count`, so doc 04's invariant — top-level VoID equals `/count` — holds by construction.

## 13. `/tpf`

The QPF/brTPF hypermedia route, unchanged in semantics from `/qpf` except that **union
rows are no longer tagged `urn:x-kgf:union`**; they are served untagged, in the
document's default graph. The measurement behind that correction, and what it costs, are
in [`graph-semantics.md`](graph-semantics.md) §"What each QPF request returns".

It declares the same vocabulary as every other route (§6) and renders its pages in the
same shell as every other page (`kgf.Html.page`), because a compatibility layer is still
part of this server rather than a second site.

## 14. Errors

RFC 9457 `application/problem+json` with doc 03 §3.6.1's **closed** code set.
`type` is `about:blank` and `title` the status reason phrase. The codes that arise here:

`bad_term_syntax`, `malformed_request`, `cap_exceeded`, `stale_cursor`,
`unsupported_format`, `not_found`, `method_not_allowed` (with `Allow`), `not_acceptable`,
`payload_too_large`, `unsupported_media_type`, `internal_error`,
`capability_not_available` (§3), `pattern_not_supported` (§6). `rate_limited` is not
raised: this server rate-limits nothing, and the gateway in front of it answers for that.

## 15. Build order

1. Identity, the URL space, problem responses, representation negotiation, cursors.
2. `/fragment`, `/count`, `/describe`, with `g` and bindings.
3. `/graphs`, `/terms`.
4. `/manifest`, `/void`, `/schema`, `/summary`.
5. `/tpf`: the QPF route moved, union rows untagged, `interop/comunica/test.mjs` rerun
   against it — including a bare pattern that spans several pages, which is the case the
   tagging correction exists for.

## 16. Where a client cannot yet read every count

`kgf-sparql`'s native KGF source rejects this server's larger counts. Its `readCount`
(`packages/actor-query-source-identify-kgf/lib/KgfClient.ts`) requires
`Number.isSafeInteger(count.value)`, so any `/count` above 2^53 − 1 raises *Invalid KGF
count response* and the query fails. Measured 2026-11-19 against this server:
`GET /count?p=<kwg-ont:cellID>` answers `9223372036854775806` and
`SELECT ?s ?id WHERE { ?s kwg-ont:cellID ?id } LIMIT 5` fails at that response, after the
dataset and service descriptors were read successfully.

The server is doing what doc 03 section 3.4.4 and doc 07 section 7.5 item 34(b) decided:
a count is a JSON number of any size, in full digits, and a reader that rounds keeps the
value within one part in 10^16, "which is harmless for planning". A client that records a
count as exact must parse it exactly; a planner need not. So the check belongs on the
client: accepting any finite, non-negative number is enough for planning, and the first
computed graph is what made the difference visible.

Everything that does not need such a count works through that client today: the dataset
and service descriptors, `/count` under the limit, `/fragment` pages, and cursor paging —
384 rows over four pages with `--page-size 100`, no row lost or repeated.
