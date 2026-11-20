package org.renci.frink.kgf

import org.apache.jena.graph.Node
import org.apache.jena.sparql.core.Quad
import org.renci.frink.kgf.Operations.Page
import org.renci.frink.kgf.Operations.PatternRequest

/** The JSON envelopes of the read operations (KGF doc 03 section 3.4). Every one of them names the dataset and the release it answered for,
  * and carries the completeness vocabulary of section 3.6.
  */
object Envelopes:

  private def envelope(fields: Seq[(String, Json)]): Json =
    Json.Obj(Vector("dataset" -> Json.Str(Service.DatasetId), "version" -> Json.Str(Service.Version)) ++ fields)

  /** An exact count, which is every count this server reports. A JSON number of any size, in full digits. */
  def exact(value: BigInt): Json = Json.obj("value" -> Json.Num(value), "exact" -> Json.Bool(true))

  private def row(request: PatternRequest, binding: Option[Int], quad: Quad): Json =
    Json.Obj(
      Vector(
        binding.map(index => "binding" -> Json.int(index)),
        Option.when(request.subject.isEmpty)("s" -> Json.term(quad.getSubject())),
        Option.when(request.predicate.isEmpty)("p" -> Json.term(quad.getPredicate())),
        Option.when(request.obj.isEmpty)("o" -> Json.term(quad.getObject())),
        Option.when(request.scope.showsGraph)("g" -> Json.term(quad.getGraph()))
      ).flatten
    )

  def fragment(request: PatternRequest, page: Page, completeness: Completeness): Json =
    envelope(
      Vector(
        "pattern" -> request.patternJson,
        "cardinality" -> exact(page.cardinality),
        "vars" -> Json.Arr(request.vars.map(Json.Str(_)))
      ) ++ request.scope.raw.map(raw => "g" -> Json.Str(raw)) ++
        Vector("rows" -> Json.Arr(page.rows.map((binding, quad) => row(request, binding, quad)))) ++
        completeness.fields
    )

  def count(request: PatternRequest, total: BigInt): Json =
    envelope(
      Vector("pattern" -> request.patternJson) ++ request.scope.raw.map(raw => "g" -> Json.Str(raw)) ++
        Vector("count" -> exact(total)) ++ Completeness.Complete.fields
    )

  def counts(request: PatternRequest, counts: Seq[(Int, BigInt)]): Json =
    envelope(
      Vector("pattern" -> request.patternJson) ++ request.scope.raw.map(raw => "g" -> Json.Str(raw)) ++
        Vector(
          "counts" -> Json.Arr(counts.map((index, total) => Json.obj("binding" -> Json.int(index), "count" -> exact(total))))
        ) ++ Completeness.Complete.fields
    )

  def describe(resource: Node, direction: String, page: Page, completeness: Completeness): Json =
    envelope(
      Vector(
        "resource" -> Json.Str(Term.encode(resource)),
        "direction" -> Json.Str(direction),
        "cardinality" -> exact(page.cardinality),
        "vars" -> Json.arr(Json.Str("s"), Json.Str("p"), Json.Str("o")),
        "rows" -> Json.Arr(
          page.rows.map((_, quad) =>
            Json.obj("s" -> Json.term(quad.getSubject()), "p" -> Json.term(quad.getPredicate()), "o" -> Json.term(quad.getObject()))
          )
        )
      ) ++ completeness.fields
    )

  /** `/graphs`: the level graphs with their counts. `triples` and `memberships` are equal here, because every quad is in exactly one level
    * graph, and the unnamed graph is never listed.
    */
  def graphs(rows: Seq[(Node, BigInt)], total: BigInt, completeness: Completeness): Json =
    envelope(
      Vector(
        "triples" -> Json.Num(Model.Queryable.triples),
        "memberships" -> Json.Num(Model.Queryable.triples),
        "cardinality" -> exact(total),
        "graphs" -> Json.Arr(rows.map((graph, count) => Json.obj("g" -> Json.term(graph), "count" -> Json.Num(count))))
      ) ++ completeness.fields
    )

  def terms(prefix: String, role: Inventory.Role, rows: Seq[Node], total: BigInt, completeness: Completeness): Json =
    envelope(
      Vector(
        "prefix" -> Json.Str(prefix),
        "role" -> Json.Str(role.name),
        "cardinality" -> exact(total),
        "terms" -> Json.Arr(
          rows.map(term => Json.obj("term" -> Json.term(term), "roles" -> Json.Arr(Inventory.rolesOf(term).map(Json.Str(_)))))
        )
      ) ++ completeness.fields
    )

  def termCount(prefix: String, role: Inventory.Role, counts: Seq[(Inventory.Role, BigInt)]): Json =
    envelope(
      Vector(
        "prefix" -> Json.Str(prefix),
        "role" -> Json.Str(role.name),
        "count" -> exact(counts.toMap.getOrElse(role, BigInt(0))),
        "counts" -> Json.Obj(counts.map((each, value) => each.name -> Json.Num(value)))
      ) ++ Completeness.Complete.fields
    )
