# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

A [Quad Pattern Fragments](https://linkeddatafragments.org/specification/quad-pattern-fragments/) server that exposes the entire S2 Geometry cell hierarchy (levels 0–30, ~10^19 cells) as a virtual RDF dataset. No data is stored: every quad is computed on demand from the `s2-geometry` library. The vocabulary mirrors KnowWhereGraph / SAWGraph spatialkg (`kwg:`, `kwgont:`, `geo:`, `spatial-full:`). Live server: https://frink.apps.renci.org/s2/qpf

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
```

Runtime env vars: `HTTP_PORT` (default 8080) and `QPF_SERVER_LOCATION` (public base URL used to build Hydra links/paging URIs; default `http://localhost:8080`, `/qpf` is appended).

Publishing a GitHub release triggers `.github/workflows/publish-docker-image.yml`, which builds a Docker image with `scala-cli --power package --docker` and pushes to ghcr.io.

## Architecture

Request flow (`src/main/scala/org/renci/frink`):

1. `Endpoints.qpfServerEndpoint` (tapir, served by Netty sync server under Ox in `Main`) parses `subject`/`predicate`/`object`/`graph` into `Types.Term` using the Hydra explicit representation (`"lex"@lang`, `"lex"^^dt`, `?var`, bare IRI; blank nodes rejected, literals only allowed as object). Unbound terms become Jena variables in a `Quad` pattern.
2. `s2.S2Graph.quads(pattern)` returns `Either[String, SizedIterator[Quad]]`. A `Left` means the pattern can't be answered and becomes **501 Not Implemented** — the server must never claim "no matches" for something it simply can't compute.
3. `qpf.QuadPatternFragment.qpf` drops `(page-1) * 100` quads and takes 100, and `FragmentMetadata` adds the Hydra/VoID control quads (total count, first/prev/next links, search template) in a metadata graph.
4. Tapir content negotiation serializes via Jena to TriG, N-Quads, or JSON-LD, or to HTML via `qpf.WebUI` (scalatags). The `from*` decoders in `Types.DatasetGraphUtils` are intentionally `???`; only encoding is used.

### SizedIterator: why paging works over an effectively infinite graph

`Util.SizedIterator` is the core abstraction: an iterator with an **exact** `BigInt` size and a `drop` that skips without enumerating. Everything in `S2Graph` is composed from these so that a deep page (e.g. offset 10^18) is cheap:

- `MultiSizedIterator` concatenates segments, skipping whole segments by size.
- `FanOutIterator` expands each source item into exactly `fanOut` items, so drops divide through to the source. `descriptionQuads` relies on every cell at a level producing the same number of matching triples (computed from `sampleDescriptions` of the first cell at each level).
- `CellRangeIterator` walks consecutive cells in Hilbert order using `S2CellId.advance`.
- `ContainmentIterator` enumerates descendants of a cell at a level, optionally pairing each with every intervening ancestor.
- `allTouches` splits each level into runs of 8-neighbor cells (fan-out) and the special face-corner cells (7 neighbors; 4 at level 0), precomputed in `cornerTouches`.

Sizes must be exact and `drop(n).iterator` must equal `iterator.drop(n)`; otherwise the last page comes up short or quads are lost. `S2GraphSpec` checks this closed-form (see "count every kind of pattern exactly" and "report sizes and pages consistently"), so any change to the triples a cell produces requires updating those expected counts.

### S2Graph data model

- Named graph per level: `kwg:s2.level{N}`. A quad lives in the graph of its subject cell's level, except containment quads (`sfWithin`/`sfContains`), which live in the **child** cell's level graph. A graph term that isn't a level graph (including the advertised default graph) matches all levels.
- `quads` concatenates, in a fixed order: cell + geometry descriptions, `sfWithin`, `sfContains`, `sfTouches` (same-level neighbors only, like spatialkg), and `spatial-full:connectedTo` (the union of the three relations, re-emitted with that predicate). Keep this order stable — page contents depend on it.
- Only canonical IRIs name a cell (`toS2Cell`/`toGeometryCell` round-trip check): level in the IRI must match the cell's level and the ID must be a plain unsigned decimal.
- Subject-bound patterns generate descriptions directly. Object-bound patterns where the object is a shared class (`ClassNodes`) use fan-out; any other object must be mapped back to candidate cells (`cellsNamedBy`: geometry IRIs, labels, cell IDs, WKT centroid) and then filtered. `geo:hasMetricArea` lookups scan an entire level and are refused (`Left`) above `MaxAreaSearchLevel` (8). A new triple whose object is shared across many cells must be added to `ClassNodes`; one with a cell-specific literal object needs a reverse lookup in `cellsNamedBy`.
- `wkt` builds lon/lat polygons with special handling for cells containing a pole and for cells crossing the antimeridian (split into `MULTIPOLYGON`).
