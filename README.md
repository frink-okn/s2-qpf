# S2 Grid: a Knowledge Graph Fragments server over S2 Geometry

The entire [S2 Geometry](http://s2geometry.io/) cell hierarchy — levels 0 to 30, about
1.36 x 10^21 triples — served as RDF over the
[Knowledge Graph Fragments](https://github.com/frink-okn/kgf) API. Nothing is stored:
every quad is computed from the `s2-geometry` library when it is asked for, and every
count is exact and in closed form, including the ones past 2^64.

**Live server:** https://frink.apps.renci.org/s2/ — the service descriptor, with the
dataset at https://frink.apps.renci.org/s2/s2. *(The KGF surface replaces the former
`/s2/qpf` route. The doubled segment is the gateway's `/s2` prefix plus the dataset's own
id, which keeps the whole documented URL space reachable; see
[docs/kgf.md](docs/kgf.md).)*

## What it serves

The KGF **core profile** — `fragment` (by GET, and by QUERY or POST with bindings),
`count`, `describe`, and the description surface `manifest`, `void`, `schema`,
`summary` — plus the **`graphs`** capability (one named graph per level, the quad view,
`GET /graphs`) and **`terms`** (a dictionary prefix scan, and exact counts for any
prefix in closed form, which is the namespace inventory a federation index needs from a
graph that can publish no membership filters).

Paths below are relative to the service base — the public base the server emits links
under, which in production is `https://frink.apps.renci.org/s2`:

```
/                                 service descriptor: caps, budgets, this dataset
/healthz                          liveness; reads nothing
/s2                               dataset descriptor: identity, releases, links
/s2/latest/...                    307 to the current release, method-preserving
/s2/v/{version}/fragment          a quad pattern, with bindings by QUERY or POST
/s2/v/{version}/count             exact cardinalities, per binding row on request
/s2/v/{version}/describe          one IRI's neighbourhood
/s2/v/{version}/graphs            the 31 level graphs with exact counts
/s2/v/{version}/terms             prefix scans and closed-form prefix counts
/s2/v/{version}/schema            the partition tree, paged
/s2/v/{version}/void              VoID, serialized per request
/s2/v/{version}/summary           the summary card, Markdown or JSON
/s2/v/{version}/manifest          release identity, capabilities, prefixes
/s2/v/{version}/tpf               Quad Pattern Fragments with bindings (brTPF)
```

`format=json` (the default) `nq` `trig` `ttl` `jsonld` `html` on `/fragment`; every
operation also answers `html`. Errors are RFC 9457 problems with KGF's coded vocabulary.
One pattern family is refused rather than answered expensively: finding cells from an
exact `geo:hasMetricArea` value deeper than level 8, which would mean scanning that
whole level. [docs/kgf.md](docs/kgf.md) is the contract in full.

## Running it

- `KGF_PUBLIC_BASE` — the base every emitted link carries, e.g.
  `https://frink.apps.renci.org/s2` (default `http://localhost:8080`). The router is
  mounted at `/` and strips nothing: a gateway that routes a prefix here removes it
  before the request arrives, and this puts it back on the way out.
- `KGF_CONTENT_DIGEST` — the release's content digest, where the deployment knows the
  container image digest. Without it the server hashes the code it is running.
- `HTTP_PORT` — default 8080.

## Querying it

With [kgf-sparql](https://github.com/frink-okn/kgf-sparql), whose native source reads
counts before pages, pushes bindings over HTTP QUERY, and follows cursors:

```shell
kgf-sparql -f query.rq --source kgf@https://frink.apps.renci.org/s2/s2 --out results.srj
```

With stock [Comunica](https://comunica.dev) against the TPF route, as a `brtpf` source
so that joins send bindings to the server:

```javascript
new QueryEngine().queryBindings(query, {
  sources: [{ type: 'brtpf', value: 'https://frink.apps.renci.org/s2/s2/latest/tpf' }],
});
```

No other configuration is needed, and Comunica's `unionDefaultGraph` context flag must
stay off. A pattern outside any `GRAPH` reads every level, because the page metadata
declares the union as the default graph and Comunica requests it by that name; `GRAPH ?g`
lists the level graphs. The graph semantics, shared with KGF, are in
[docs/graph-semantics.md](docs/graph-semantics.md).

Bindings on the TPF route travel in the URL, so a proxy in front of the server must
accept long request lines: Comunica's 64 bindings per request can exceed 7 KB. On that
route a quad matching several binding rows is included only for the first, so a page can
hold fewer than 100 quads and the total is an upper bound. The KGF route has no such
caveat: a row there carries the index of the binding it joins with, so nothing is
deduplicated and the cardinality is exact.

`interop/comunica` checks compatibility with Comunica 5.3.0 against a running server:

```shell
npm ci --prefix interop/comunica
node interop/comunica/test.mjs http://localhost:8080/s2/v/v2.0.0/tpf
```

## Developer quick start

If you don't have Scala CLI installed yet, please follow these [installation instructions](https://scala-cli.virtuslab.org/install).
You can use the following commands to compile, test and run the project:

```shell
scala-cli --power compile --test . # build the project ('--test' means that tests will be also compiled)
scala-cli --power test . # run the tests
scala-cli --power run . # run the application (Main)
scala-cli --power fmt --check . # run scalaformat check on all scala files and print summary, removing '--check' fixes badly formatted files
```

To open project in the IDE (Metals / IntelliJ) run any of the `compile` or `test` command above and open the project.
IDE should detect a BSP project and import it.

Alternatively, you can use Scala CLI via a docker image:

```shell
docker run -ti --rm -v $(pwd):/app virtuslab/scala-cli compile --test /app # build the project ('--test' means that tests will be also compiled)
docker run -ti --rm -v $(pwd):/app virtuslab/scala-cli test /app # run the tests
docker run -ti --rm -p '8080:8080' -v $(pwd):/app virtuslab/scala-cli run /app # run the application (Main)
```

For more details check the [Scala CLI commands](https://scala-cli.virtuslab.org/docs/commands/basics) page.

## Packaging

Create executable that automatically downloads dependencies; requires JVM 21+ on PATH:

```shell
scala-cli --power package . -o s2-qpf
```

## Links:

- [tapir documentation](https://tapir.softwaremill.com/en/latest/)
- [tapir github](https://github.com/softwaremill/tapir)
- [bootzooka: template microservice using tapir](https://softwaremill.github.io/bootzooka/)
- [Scala CLI](https://scala-cli.virtuslab.org)
