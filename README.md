# S2 Geometry Quad Pattern Fragments server

- [S2 Geometry](http://s2geometry.io/)
- [Quad Pattern Fragments](https://linkeddatafragments.org/specification/quad-pattern-fragments/)
- [Linked Data Fragments](https://linkeddatafragments.org/)

**Live server:** https://frink.apps.renci.org/s2/qpf

## Running

- Provide server location in `QPF_SERVER_LOCATION` environment variable (e.g., `http://localhost:8080`)

## Bindings-restricted fragments

The server also answers [bindings-restricted](https://arxiv.org/abs/1608.08148) requests (brTPF, applied to quad patterns): a `values` parameter holding SPARQL `VALUES` syntax without the `VALUES` keyword restricts the pattern to quads whose values for its variables are compatible with at least one row, e.g. `subject=?cell&predicate=http://stko-kwg.geog.ucsb.edu/lod/ontology/cellID&values=(?cell) { (<http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level13.5525166171478818816>) }`. A request can have up to 1000 rows.

Clients can't discover this from the server, so tell [Comunica](https://comunica.dev) to use it:

```javascript
new QueryEngine().queryBindings(query, { sources: [{ type: 'brtpf', value: 'https://frink.apps.renci.org/s2/qpf' }] });
```

No other configuration is needed. A pattern outside any `GRAPH` reads every level, because
the page metadata declares the union as the default graph and Comunica requests it by that
name. Leave Comunica's `unionDefaultGraph` context flag off: it is unnecessary here, and
against an endpoint that declares no default graph it makes Comunica build the union
itself without removing duplicates. The graph semantics, shared with KGF, are in
[docs/graph-semantics.md](docs/graph-semantics.md).

Bindings are sent in the URL, so a proxy in front of the server needs to accept long request lines: Comunica's 64 bindings per request can exceed 7 KB.

When rows overlap, a quad matching several of them is only included for the first, so pages can have fewer than 100 quads, and the total is an upper bound.

`interop/comunica` checks compatibility with Comunica 5.3.0 against a running server:

```shell
npm ci --prefix interop/comunica
node interop/comunica/test.mjs http://localhost:8080/qpf
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
