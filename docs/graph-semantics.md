# Graph semantics: the contract shared with KGF, and where this server stands

Decided 2026-09-15 with the KGF design, then corrected the same day from tests of this
server's variants against stock Comunica 5.3.0 (the test record is below), and implemented
on the `brtpf` branch. This server is
a Quad Pattern Fragments endpoint whose data is computed, but a SPARQL client must get the
same answers from it as from a KGF dataset with named graphs, so both follow one contract.
The contract is the union-default convention of QLever, RDF4J/GraphDB, and Blazegraph.

## The contract

1. **The default graph is the union.** A SPARQL pattern outside any `GRAPH` reads every
   quad, whatever graph it is in, deduplicated to distinct triples. Here every quad is
   in exactly one level graph, so the union has no duplicates to remove.
2. **Two reserved graph IRIs, the same in every KGF dataset and here:**
   - `urn:x-kgf:union` names the union. `GRAPH <urn:x-kgf:union> { … }` must return the
     same rows as the bare pattern.
   - `urn:x-kgf:unnamed` names the unnamed graph, the triples that carry no graph.
     This server has none: every quad lives in a level graph.
3. **`GRAPH ?g` lists named graphs only**, never the unnamed graph and never the union.
   Here that means the level graphs.
4. **Totals describe the view requested.** A request with `graph` unbound counts
   memberships; a request for the union counts distinct triples. Here they are equal.

## What each QPF request returns

| `graph=` | Data quads in the page | `hydra:totalItems` |
|---|---|---|
| absent, or a variable such as `?g` (what `brtpf` sends) | every matching quad, each tagged with its level graph | all matching quads |
| `urn:x-kgf:union` | every matching triple once, **tagged `urn:x-kgf:union`** | distinct triples, here the same number |
| a level graph | that level's matching quads, tagged with it | that level's count |
| `urn:x-kgf:unnamed` | none | 0 |
| any other IRI | none | 0 |

Page metadata keeps the four-mapping search form (`rdf:subject`, `rdf:predicate`,
`rdf:object`, `sd:graph`) and declares the default graph in this shape:

```turtle
<dataset> sd:defaultDataset [ sd:defaultGraph <urn:x-kgf:union> ] .
```

**The blank-node subject is required.** Comunica reads `sd:defaultGraph` only when the
triple's subject is the exact page URL it fetched, a resource that appeared earlier in the
response as `<X> void:subset <page URL>`, an `sd:defaultDataset` value seen earlier, or a
blank node. `<dataset> sd:defaultGraph …` never qualifies here, because the dataset's
`void:subset` is the `…#fragment` resource, not the page. Without a declaration Comunica
treats the default graph as empty and makes no request at all for a bare pattern.

**Why union rows are tagged with the union IRI rather than left untagged.** Comunica sends
the identical request, `graph=urn:x-kgf:union`, in two situations and filters the answer
differently: for a bare pattern (after substituting the declared default graph) it keeps
rows tagged with that IRI *or* untagged, mapping both to the SPARQL default graph; for an
explicit `GRAPH <urn:x-kgf:union>` it keeps only rows tagged with that IRI. The server
cannot tell the two apart, so tagged rows are the only answer that satisfies both.
Rule 3 still holds: `GRAPH ?g` patterns are requested with the graph unset or as a
variable, never as the union IRI, so union-tagged rows never reach them.

**Comunica's `unionDefaultGraph` context flag stays off** because it is unnecessary: once
a default graph is declared, Comunica requests it for bare patterns instead of consulting
the flag. The flag's only other effect, keeping untagged quads in `GRAPH ?g` results,
doesn't arise, because this server never answers a `GRAPH ?g` request with untagged quads.
Against an endpoint that declares no default graph, the flag makes Comunica build the
union itself without removing duplicates (read from Comunica's `quadsToBindings`, not
tested), which is the failure mode KGF's design exists to avoid.

## Test record, 2026-09-15

Server variants built from a scratch copy of the `brtpf` branch, queried through
`@comunica/query-sparql` 5.3.0 as `qpf` and `brtpf` sources with no context flags. Variant
A was run as `qpf` only; the unmodified branch, run as `brtpf`, also returned 0 rows for a
bare pattern without making a request. Every query should return 8
rows, the neighbours of one level-13 cell: bare `<cell> sfTouches ?o`; the same inside
`GRAPH ?g`; a bare join to `cellID`; and the same inside `GRAPH <urn:x-kgf:union>`.

| Server variant | Bare | `GRAPH ?g` | Bare join | Explicit union |
|---|---|---|---|---|
| A. `<dataset> sd:defaultGraph <urn:x-kgf:union>` | 0, no request made | 8 | 0 | not run |
| B. Blank-node declaration; union rows still tagged per level | 0, requests `graph=urn:x-kgf:union` | 8 | 0 | not run |
| C. B, union rows untagged | 8 | 8 | 8 | **0** |
| D. B, union rows tagged `urn:x-kgf:union` | 8 | 8 | 8 | **8** |

Variant D is the contract. With the flag on, variant C returned the same results for the
bare, `GRAPH ?g`, and bare-join queries, and `{ bare } UNION { GRAPH ?g { … } }`, run as
`qpf`, returned 16 rows with `?g` bound in 8 against both C and D. A bare query followed by
`SELECT DISTINCT ?g { GRAPH ?g { … } }` on one `brtpf` engine with a shared cache listed
only `s2.level13` (variant D).

The implementation passes `interop/comunica/test.mjs`, which checks all of these with no
context flags: bare patterns and a bare brTPF join read the union and request
`graph=urn:x-kgf:union`, `GRAPH <urn:x-kgf:union>` returns the bare rows,
`GRAPH <urn:x-kgf:unnamed>` returns none, `GRAPH ?g` lists only level graphs even after the
same engine has cached the union, and metadata never appears as data, inside or outside
`GRAPH`.

## Implementation

"Before" is the `brtpf` branch before this change. That branch already repeated the
requested URL in page metadata and named the fragment `…#fragment`; on `main`, which does
neither, Comunica can't match page metadata to the page it fetched at all.

| Contract | Before | Now |
|---|---|---|
| default graph declared on a blank node under `sd:defaultDataset` | `<dataset> sd:defaultGraph <urn:ldf:defaultGraph>`, which Comunica ignored | `QuadPatternFragment` declares `QuadPatternFragment.UnionGraph` (`urn:x-kgf:union`) in the blank-node shape |
| `graph=<union>` returns each triple once, tagged `urn:x-kgf:union` | rows tagged with their level graphs | `S2Graph.quads(pattern)`: `urn:x-kgf:union`, and `urn:ldf:defaultGraph` as an alias (`S2Graph.UnionGraphs`), match every level, with rows retagged as the union |
| any IRI other than a level graph or the union matches nothing | `S2Graph.quads(pattern)` matched every level for any non-level graph (`toS2Level(...).getOrElse(Levels)`), and `S2Graph.quads(pattern, bindings)` turned a non-level graph in the pattern into a variable matching every level | a graph that is neither a variable, a level graph, nor the union gives an empty fragment with total 0, including `urn:x-kgf:unnamed` and levels past 30, and never a 501 |
| a union pattern with bindings returns each triple once | see the row above | `S2Graph.quads(pattern, bindings)` matches the rows with the graph as a variable, so repeated quads are removed while they are still in their level graphs, and then retags the answer as the union |
| a brTPF row binding `?g` to anything but a level graph matches nothing | already right for non-level graphs | still right, and a row binding `?g` to the union IRI or its alias also matches nothing |
| `graph` unbound or a variable: tagged per level, total = quad count | same | unchanged |
| `GRAPH ?g` lists level graphs only | same | unchanged |
| HTML view links graph names | links each row's graph | unchanged; union rows link to `graph=urn:x-kgf:union` |

Tests added or changed:

- `S2GraphSpec`: the union answered by name and by its alias, with the same triples and
  size as the unbound graph; the unnamed graph, an arbitrary IRI, and a level past 30
  matching nothing, even for an area lookup that would otherwise be refused; a binding row
  naming a level graph, the union, the alias, the unnamed graph, or a literal matching only
  the level graph; a union pattern with overlapping rows answering each triple once,
  tagged as the union.
- `EndpointsSpec`: the blank-node declaration; `graph=` set to the union, a level graph,
  the unnamed graph, and another IRI, with totals.
- `interop/comunica/test.mjs`: the Comunica checks listed in the test record.

## If this server grows a KGF-style API

The same contract, in KGF's parameter forms on `/fragment` and `/count`:

| `g` | Meaning here |
|---|---|
| absent, or `<urn:x-kgf:union>` | all levels, one row per triple |
| `<level graph>` | that level |
| `<urn:x-kgf:unnamed>` | empty |
| `*` | the quad view: every row with a `g` column holding its level graph |

Exact counts for every form already exist: `SizedIterator` sizes are exact and
`drop` is positional, so a KGF cursor can be a page position.
