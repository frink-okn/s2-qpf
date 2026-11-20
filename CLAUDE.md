# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

A [Knowledge Graph Fragments](https://github.com/frink-okn/kgf) (KGF) server that exposes the entire S2 Geometry cell hierarchy (levels 0–30, ~10^19 cells, ~1.36 × 10^21 triples) as a virtual RDF dataset. No data is stored: every quad is computed on demand from the `s2-geometry` library, and every count is exact and in closed form. The vocabulary mirrors KnowWhereGraph / SAWGraph spatialkg (`kwg:`, `kwg-ont:`, `geo:`, `spatial-full:`). Live server: https://frink.apps.renci.org/s2/

This is a **computed graph** in KGF's terms (`../kgf` doc 05 §5.8): it speaks KGF because it can honor the cost contract, not because it holds a bundle. [`docs/kgf.md`](docs/kgf.md) is the contract this server meets — the URL space, the identity of a release, the capabilities it declares and refuses, the cursors, the caps, and every place it diverges from doc 03 and why. Change that document before changing the surface.

## Commands

Built with Scala CLI (no sbt). Dependencies, Scala version, and `publish.version` all live in `//> using` directives in `project.scala`. JVM 21.

```shell
scala-cli --power compile --test .                 # compile main + tests
scala-cli --power test .                           # run all tests (what CI runs)
scala-cli --power test . --test-only 'org.renci.frink.s2.S2GraphSpec'           # one suite
scala-cli --power test . --test-only 'org.renci.frink.s2.S2GraphSpec' -- -z "count every kind"  # tests whose name contains a substring
scala-cli --power fmt --check .                    # scalafmt check (drop --check to fix); maxColumn 140
scala-cli --power run .                            # serve on http://localhost:8080/qpf
scala-cli --power package . -o s2-qpf              # launcher executable
node interop/comunica/test.mjs http://localhost:8080/s2/v/v2.0.0/tpf  # Comunica 5.3.0 conformance against a running server (npm ci --prefix interop/comunica first)
```

Runtime env vars: `HTTP_PORT` (default 8080), `KGF_PUBLIC_BASE` (the base every emitted link carries; default `http://localhost:8080`) and `KGF_CONTENT_DIGEST` (the release's content digest, where the deployment knows the container image digest; otherwise the server hashes its own classpath). `publish.version` in `project.scala` is the dataset's release — `kgf.Service.Release` must match it, which `ServiceSpec` checks.

Publishing a GitHub release triggers `.github/workflows/publish-docker-image.yml`, which builds a Docker image with `scala-cli --power package --docker` and pushes to ghcr.io.

## Architecture

Two routes answer over one graph (`src/main/scala/org/renci/frink`): the KGF operations in `kgf/`, and the TPF/brTPF compatibility route in `Endpoints.scala`. `Endpoints.all` assembles every route in matching order and `Main` serves it (tapir, Netty sync server under Ox).

**The KGF route** (`kgf/Endpoints.scala`; `docs/kgf.md`):

1. Parameters are read from the `ServerRequest` rather than declared one by one, because an operation takes up to a dozen of them and the negotiation, the caps and the cursor are shared. Terms are KGF doc 03 §3.3 syntax — `<iri>`, a CURIE against `Service.Prefixes`, `"lex"@lang`, `"lex"^^dt`, `?var` — parsed by `kgf.Term`.
2. `kgf.Operations.PatternRequest` holds the parsed pattern, the `g` scope (union, a named graph, or `*` for the quad view) and the object filters. `o.dt` becomes a predicate restriction, exactly, because each predicate here has one object datatype; `o.lang` matches nothing. The restriction is kept apart from the pattern the client sent, so a row still carries the position the client left unbound.
3. `Operations.quads` calls `S2Graph.quads` and turns a `Left` into `501 pattern_not_supported`. Bindings go through `Operations.restricted`, which answers one row per (input row, quad) with a `binding` column, so nothing is deduplicated and the cardinality is exactly the sum of the per-row counts.
4. A page is a position: `Cursor` encodes it with a fingerprint of (content digest, operation, canonical request), and anything else is `400 stale_cursor`. `Envelopes` builds the JSON, `Rdf` the RDF representations (the union untagged, a scope tagged, the quad view tagged per row), `Html` the HTML view of the same JSON.
5. `kgf.Model` is the closed-form description model every statistic comes from — `/manifest` counts, `/void` (`kgf.Void`), `/schema` (`kgf.Schema`), `/summary` (`kgf.Descriptors`). Each number is the size of the pattern that answers the matching `/count`, which is how doc 04's invariant holds by construction; `ModelSpec` checks each against that pattern. `kgf.Inventory` answers `/terms` in closed form over the cell-id progressions.
6. Errors are RFC 9457 problems with doc 03 §3.6.1's closed code set (`kgf.Problem`), whatever the client asked to accept.

**The TPF route** (`/{dataset}/v/{version}/tpf`):

1. `Endpoints.tpfServerEndpoint` parses `subject`/`predicate`/`object`/`graph` into `Types.Term` using the Hydra explicit representation (bare IRIs, not bracketed; blank nodes rejected, literals only as object). It declares no prefixes of its own: `kgf.Service.Prefixes` is the one vocabulary this server declares — the manifest's, and the OKN shared table's — and `QuadPatternFragment.ControlPrefixes` adds only this document's hypermedia (`hydra`, `dct`, `foaf`, `sd`). Unbound terms become Jena variables in a `Quad` pattern; named variables keep their names because brTPF bindings refer to them. An optional `values` parameter is parsed into `qpf.Bindings`.
2. `S2Graph.quads(pattern)` returns `Either[String, SizedIterator[Quad]]`, or with bindings `S2Graph.quads(pattern, bindings)` returns `Either[String, Matches]`. A `Left` becomes **501 `pattern_not_supported`** — the server must never claim "no matches" for something it simply can't compute.
3. `qpf.QuadPatternFragment.qpf` drops `(page-1) * 100` positions and takes 100, and `FragmentMetadata` adds the Hydra/VoID control quads (total count, first/prev/next links, search template) in a metadata graph.
4. Tapir content negotiation serializes via Jena to TriG, N-Quads, or JSON-LD, or to HTML via `qpf.WebUI` (scalatags), which builds its page in `kgf.Html.page` — the shell and stylesheet **every** resource of this server shares, so one server does not look like two sites. Nothing is loaded from a CDN. Union rows are written into the document's default graph, not tagged with the union IRI — see `docs/graph-semantics.md`. The `from*` decoders in `Types.DatasetGraphUtils` are intentionally `???`; only encoding is used.

A tapir endpoint that does not declare a path input matches **every** path, so each route checks `request.pathSegments` for the shape it serves and the catch-all (`kgf.Endpoints.unmatched`, last in the list) answers the service descriptor for `/`, a coded `405` with `Allow` for a known operation under another method, and a coded `404` otherwise.

### SizedIterator: why paging works over an effectively infinite graph

`Util.SizedIterator` is the core abstraction: an iterator with an **exact** `BigInt` size and a `drop` that skips without enumerating. Everything in `S2Graph` is composed from these so that a deep page (e.g. offset 10^18) is cheap:

- `MultiSizedIterator` concatenates segments, skipping whole segments by size.
- `FanOutIterator` expands each source item into exactly `fanOut` items, so drops divide through to the source. `descriptionQuads` relies on every cell at a level producing the same number of matching triples (computed from `sampleDescriptions` of the first cell at each level).
- `CellRangeIterator` walks consecutive cells in Hilbert order using `S2CellId.advance`.
- `ContainmentIterator` enumerates descendants of a cell at a level, optionally pairing each with every intervening ancestor.
- `allTouches` splits each level into runs of 8-neighbor cells (fan-out) and the special face-corner cells (7 neighbors; 4 at level 0), precomputed in `cornerTouches`.

Sizes must be exact and `drop(n).iterator` must equal `iterator.drop(n)`; otherwise the last page comes up short or quads are lost. `S2GraphSpec` checks this closed-form (see "count every kind of pattern exactly" and "report sizes and pages consistently"), so any change to the triples a cell produces requires updating those expected counts.

### Bindings-restricted QPF (brTPF) and Comunica

`qpf.Bindings` parses Comunica's `values=` (SPARQL `VALUES` without the keyword) with Jena, substitutes each row into the pattern, and drops duplicate or subsumed restrictions. `Bindings.matches` concatenates each restriction's matches; since restrictions with `UNDEF` can overlap, `Matches.positions` is a `SizedIterator[Option[Quad]]` where a quad already matched by an earlier restriction is `None`. That keeps page skipping positional (no dedup counting over huge iterators) at the cost of short pages, and `Matches.exact` is false when counts are only an upper bound. `S2Graph.quads(pattern, bindings)` adds that a row binding the graph to anything but a level graph matches nothing, and matches a union pattern with a variable graph before retagging, so overlaps are found among level quads.

Comunica compatibility depends on details that are easy to break:

- Comunica recognizes a page's metadata only by exact string match against the URL it fetched. `FragmentMetadata.pageURL` therefore echoes the raw request query (from Netty's request line, not tapir's re-encoded `Uri`), and `next`/`previous` are built by editing that raw query.
- Comunica picks the metadata graph via the single resource that has the page as `void:subset`. The fragment is named `…#fragment` so the dataset's `void:subset` never also points at the page.
- Comunica reads `sd:defaultGraph` only from a blank node (among a few subjects that don't fit here), so the union is declared as `<dataset> sd:defaultDataset [ sd:defaultGraph <urn:x-kgf:union> ]`. Without it, patterns outside `GRAPH` silently return nothing.
- The Hydra search form must keep exactly four mappings (s/p/o/`sd:graph`); `values` is appended by Comunica, not templated.
- `Main` raises Netty's request-line limit, since 64 bindings exceed the 4096-character default.

### Graph semantics

[`docs/graph-semantics.md`](docs/graph-semantics.md) states the named-graph contract
shared with KGF (union default graph, the two reserved graph IRIs, what each `graph=`
request returns and how its quads are tagged), with the Comunica tests behind it and how
this server implements it. Change the contract there before changing the code. Union rows
are served **untagged** on the TPF route: stock Comunica discards tagged union rows from a
fragment's second page on, which truncated every bare pattern at 100 rows on the deployed
server — the measurement is in that document.

### S2Graph data model

- Named graph per level: `kwg:s2.level{N}`. A quad lives in the graph of its subject cell's level, except containment quads (`sfWithin`/`sfContains`), which live in the **child** cell's level graph. Graph terms follow the contract shared with KGF in `docs/graph-semantics.md`: a variable matches every level, tagged per level; `urn:x-kgf:union` (the declared default graph, alias `urn:ldf:defaultGraph`) matches every level with rows tagged as the union; any other graph, including `urn:x-kgf:unnamed`, matches nothing (empty fragment, not 501).
- `quads` concatenates, in a fixed order: cell + geometry descriptions, `sfWithin`, `sfContains`, `sfTouches` (same-level neighbors only, like spatialkg), and `spatial-full:connectedTo` (the union of the three relations, re-emitted with that predicate). Keep this order stable — page contents depend on it.
- A pattern with the same term or variable in two positions matches nothing: no term occurs in two positions of any quad in this graph.
- Only canonical IRIs name a cell (`toS2Cell`/`toGeometryCell` round-trip check): level in the IRI must match the cell's level and the ID must be a plain unsigned decimal.
- Subject-bound patterns generate descriptions directly. Object-bound patterns where the object is a shared class (`ClassNodes`) use fan-out; any other object must be mapped back to candidate cells (`cellsNamedBy`: geometry IRIs, labels, cell IDs, WKT centroid) and then filtered. `geo:hasMetricArea` lookups scan an entire level and are refused (`Left`) above `MaxAreaSearchLevel` (8). A new triple whose object is shared across many cells must be added to `ClassNodes`; one with a cell-specific literal object needs a reverse lookup in `cellsNamedBy`.
- `wkt` builds lon/lat polygons with special handling for cells containing a pole and for cells crossing the antimeridian (split into `MULTIPOLYGON`).
