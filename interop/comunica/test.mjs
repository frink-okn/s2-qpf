import assert from 'node:assert/strict';
import { QueryEngine } from '@comunica/query-sparql';

const endpoint = process.argv[2];
assert(endpoint, 'usage: node test.mjs http://localhost:8080/qpf');

const resource = 'http://stko-kwg.geog.ucsb.edu/lod/resource/';
const ontology = 'http://stko-kwg.geog.ucsb.edu/lod/ontology/';
const york = `${resource}s2.level13.5525166171478818816`;

const requested = [];
const context = {
  sources: [{ type: 'brtpf', value: endpoint }],
  fetch: (url, init) => {
    requested.push(new URL(String(url)));
    return fetch(url, init);
  },
};

async function rows(query, variables) {
  requested.length = 0;
  const bindings = await (await new QueryEngine().queryBindings(query, context)).toArray();
  return bindings.map(binding => Object.fromEntries(variables.map(variable => [variable, binding.get(variable)?.value])));
}

const negotiated = await fetch(endpoint, {
  headers: {
    accept: 'application/n-quads, application/trig;q=0.95, application/ld+json;q=0.9, application/n-triples;q=0.8, text/turtle;q=0.6',
  },
});
assert.equal(negotiated.status, 200);
assert.equal(negotiated.headers.get('content-type'), 'application/n-quads; charset=utf-8', 'Comunica\'s preferences must negotiate N-Quads');
await negotiated.arrayBuffer();

const touches = await rows(`SELECT * WHERE { GRAPH <${resource}s2.level1> { ?s <${ontology}sfTouches> ?o } }`, ['s', 'o']);
assert.equal(touches.length, 168, 'Comunica must follow hydra:next through every page of a fragment');

for (const pattern of ['?s <http://rdfs.org/ns/void#subset> ?o', 'GRAPH ?g { ?s <http://rdfs.org/ns/void#subset> ?o }']) {
  assert.deepEqual(await rows(`SELECT * WHERE { ${pattern} }`, ['s']), [], `fragment metadata must not be taken for data: ${pattern}`);
}

// The default graph is the union of the level graphs, declared by the server, with no client configuration
const touchesYork = `<${york}> <${ontology}sfTouches> ?o`;
const bare = await rows(`SELECT * WHERE { ${touchesYork} }`, ['o']);
assert.equal(bare.length, 8, 'a pattern outside any GRAPH must read the union');
assert(requested.some(url => url.searchParams.get('graph') === 'urn:x-kgf:union'), 'Comunica must request the declared union');
assert.equal((await rows(`SELECT * WHERE { GRAPH <urn:x-kgf:union> { ${touchesYork} } }`, ['o'])).length, 8, 'the union can be named');
const named = await rows(`SELECT * WHERE { GRAPH ?g { ${touchesYork} } }`, ['g', 'o']);
assert.deepEqual(new Set(named.map(row => row.g)), new Set([`${resource}s2.level13`]), 'GRAPH ?g must list level graphs');
assert.deepEqual(new Set(named.map(row => row.o)), new Set(bare.map(row => row.o)));
assert.equal((await rows(`SELECT * WHERE { GRAPH <urn:x-kgf:unnamed> { ${touchesYork} } }`, ['o'])).length, 0);

const sharedEngine = new QueryEngine();
await (await sharedEngine.queryBindings(`SELECT * WHERE { ${touchesYork} }`, context)).toArray();
const graphsAfterUnion = (await (await sharedEngine.queryBindings(`SELECT DISTINCT ?g WHERE { GRAPH ?g { ${touchesYork} } }`, context)).toArray())
  .map(binding => binding.get('g').value);
assert.deepEqual(graphsAfterUnion, [`${resource}s2.level13`], 'GRAPH ?g must never list the union, even with the union cached');

const bareJoin = await rows(`SELECT ?neighbor ?id WHERE { <${york}> <${ontology}sfTouches> ?neighbor . ?neighbor <${ontology}cellID> ?id }`, ['neighbor', 'id']);
assert.equal(bareJoin.length, 8);
assert(requested.some(url => url.searchParams.has('values') && url.searchParams.get('graph') === 'urn:x-kgf:union'), 'a join outside GRAPH must push bindings into the union');

const neighbors = await rows(`
  SELECT ?neighbor ?id WHERE {
    GRAPH ?g1 { <${york}> <${ontology}sfTouches> ?neighbor }
    GRAPH ?g2 { ?neighbor <${ontology}cellID> ?id }
  }
`, ['neighbor', 'id']);
assert.equal(neighbors.length, 8);
for (const { neighbor, id } of neighbors) assert.equal(neighbor, `${resource}s2.level13.${id}`);
assert(requested.some(url => url.searchParams.has('values')), 'the join must push bindings into the server with values=');

// 24 bindings, and 384 matches over four pages of a bindings-restricted fragment
const children = await rows(`
  SELECT ?cell ?child WHERE {
    GRAPH <${resource}s2.level1> { ?cell a <${ontology}S2Cell_Level1> }
    GRAPH <${resource}s2.level3> { ?child <${ontology}sfWithin> ?cell }
  }
`, ['cell', 'child']);
assert.equal(children.length, 384);
assert.equal(new Set(children.map(row => row.child)).size, 64 * 6, 'every level 3 cell is within one level 1 cell');
assert(requested.some(url => url.searchParams.has('values') && url.searchParams.has('page')), 'bindings-restricted fragments must page');

// Comunica's default 64 bindings per request make URLs longer than Netty's default limit
const grandchildren = await rows(`
  SELECT ?cell ?child WHERE {
    GRAPH <${resource}s2.level3> { ?cell a <${ontology}S2Cell_Level3> }
    GRAPH <${resource}s2.level4> { ?child <${ontology}sfWithin> ?cell }
  }
`, ['cell', 'child']);
assert.equal(grandchildren.length, 384 * 4);
assert(requested.some(url => url.href.length > 4096), 'some requests must carry more bindings than fit in 4096 characters');

console.log('Comunica 5.3.0 paging, metadata, graph semantics, bind joins, and bindings-restricted paging passed');
