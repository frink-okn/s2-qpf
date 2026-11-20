package org.renci.frink.kgf

import org.renci.frink.s2.S2Graph

/** The self-describing surface: the service descriptor, the dataset descriptor, the bundle manifest and the summary card (KGF doc 03
  * sections 3.2 and 3.4.10). A client that reads these knows the capabilities, the caps and budgets, the prefixes and the release.
  */
object Descriptors:

  /** The container image the release workflow publishes, which is the artifact a computed graph's release is (KGF doc 05 section 5.8) */
  val Image = "ghcr.io/frink-okn/s2-qpf"

  private def links: Json =
    Json.Obj(Service.Operations.map(operation => operation -> Json.Str(Service.link(Service.operationPath(operation)))))

  def service: Json =
    Json.obj(
      "datasets" -> Json.arr(
        Json.obj(
          "id" -> Json.Str(Service.DatasetId),
          "title" -> Json.Str(Service.Title),
          "description" -> Json.Str(Service.Description),
          "triples" -> Json.Num(Model.Queryable.triples),
          "current" -> Json.Str(Service.Version),
          "class" -> Json.Str("computed"),
          "capabilities" -> Json.Arr(Service.Capabilities.map(Json.Str(_))),
          "url" -> Json.Str(Service.link(Service.DatasetPath)),
          "links" -> links
        )
      ),
      "caps" -> Limits.caps,
      "budgets" -> Limits.budgets,
      "implementation" -> Json.obj("s2-qpf" -> Json.Str(Service.Release), "protocol" -> Json.Str("1"))
    )

  def dataset: Json =
    Json.obj(
      "id" -> Json.Str(Service.DatasetId),
      "dataset_iri" -> Json.Str(Service.DatasetIRI),
      "title" -> Json.Str(Service.Title),
      "description" -> Json.Str(Service.Description),
      "class" -> Json.Str("computed"),
      "homepage" -> Json.Str(Service.Homepage),
      "current" -> Json.Str(Service.Version),
      "releases" -> Json.arr(
        Json.obj(
          "version" -> Json.Str(Service.Version),
          "content_digest" -> Json.Str(Service.ContentDigest),
          "url" -> Json.Str(Service.link(s"${Service.VersionPath}/")),
          "links" -> links
        )
      )
    )

  /** The manifest keeps the ordinary shape, with the generator in place of a bundle's files: there are none, and the manifest says so
    * rather than leaving the block out (KGF doc 05 section 5.8).
    */
  def manifest: Json =
    Json.obj(
      "id" -> Json.Str(Service.DatasetId),
      "dataset_iri" -> Json.Str(Service.DatasetIRI),
      "version" -> Json.Str(Service.Version),
      "content_digest" -> Json.Str(Service.ContentDigest),
      "class" -> Json.Str("computed"),
      "formats" -> Json.obj("manifest" -> Json.Str("1"), "generator" -> Json.Str("1")),
      "title" -> Json.Str(Service.Title),
      "description" -> Json.Str(Service.Description),
      "license" -> Json.Str(Service.License),
      "homepage" -> Json.Str(Service.Homepage),
      "counts" -> Json.obj(
        "triples" -> Json.Num(Model.Queryable.triples),
        "subjects" -> Json.Num(Model.Queryable.subjects),
        "predicates" -> Json.int(Model.Predicates.size),
        "classes" -> Json.int(Model.Classes.size)
      ),
      "capabilities" -> Json.Obj(Service.Capabilities.map(capability => capability -> Json.Obj(Vector.empty))),
      "prefixes" -> Json.Obj(Service.Prefixes.map((prefix, namespace) => prefix -> Json.Str(namespace))),
      "artifacts" -> Json.obj(
        "generator" -> Json.obj(
          "release" -> Json.Str(Service.Release),
          "digest" -> Json.Str(Service.ContentDigest),
          "image" -> Json.Str(s"$Image:${Service.Release}"),
          "source" -> Json.Str(Service.Homepage)
        )
      )
    )

  /** No distinct-object count. Two cells can carry the same `geo:hasMetricArea` literal and how many do is not closed-form, so this server
    * states no total for distinct objects rather than publishing an upper bound as though it were exact.
    */
  private val refusal =
    s"One pattern family is refused: finding cells from an exact `geo:hasMetricArea` value deeper than level " +
      s"${S2Graph.MaxAreaSearchLevel}, which would mean scanning every cell at that level. The request answers 501 " +
      "`pattern_not_supported` and names the level; every other pattern is answered within the cost contract."

  private def largestGraphs: Seq[(String, BigInt)] =
    Model.Levels.reverse.take(10).map(level => S2Graph.Graphs(level).getURI() -> Model.levelTriples(level))

  private def topClasses: Seq[(String, BigInt)] =
    Model.Queryable.classes.sortBy(-_.entities).take(5).map(partition => partition.term.getURI() -> partition.entities)

  private def topProperties: Seq[(String, BigInt)] =
    Model.Queryable.properties.sortBy(-_.triples).take(11).map(property => property.predicate.getURI() -> property.triples)

  def summaryJson: Json =
    Json.obj(
      "dataset" -> Json.Str(Service.DatasetId),
      "version" -> Json.Str(Service.Version),
      "view" -> Json.Str("design"),
      "counts" -> Json.obj(
        "triples" -> Json.Num(Model.Queryable.triples),
        "distinct_subjects" -> Json.Num(Model.Queryable.subjects),
        "predicates" -> Json.int(Model.Predicates.size),
        "classes" -> Json.int(Model.Classes.size)
      ),
      "graphs_total" -> Json.int(Model.Levels.size),
      "graphs" -> Json.Arr(
        largestGraphs.map((graph, count) => Json.obj("g" -> Json.Str(graph), "triples" -> Json.Num(count)))
      ),
      "top_classes" -> Json.Arr(topClasses.map((term, entities) => Json.obj("class" -> Json.Str(term), "entities" -> Json.Num(entities)))),
      "top_properties" -> Json.Arr(
        topProperties.map((term, triples) => Json.obj("property" -> Json.Str(term), "triples" -> Json.Num(triples)))
      ),
      "namespaces" -> Json.Arr(
        Service.Prefixes.map((prefix, namespace) => Json.obj("prefix" -> Json.Str(prefix), "namespace" -> Json.Str(namespace)))
      ),
      "refusals" -> Json.arr(Json.Str(refusal.replace("`", ""))),
      "links" -> links
    )

  def summaryMarkdown: String =
    val graphs = largestGraphs.map((graph, count) => s"- `$graph` ($count triples)").mkString("\n")
    val classes = topClasses.map((term, entities) => s"- `$term` ($entities entities)").mkString("\n")
    val properties = topProperties.map((term, triples) => s"- `$term` ($triples triples)").mkString("\n")
    val namespaces = Service.Prefixes.map((prefix, namespace) => s"- `$prefix:` `$namespace`").mkString("\n")
    s"""# ${Service.Title}
       |
       |## Dataset facts
       |
       |- Dataset: `${Service.DatasetId}` version `${Service.Version}`, a **computed** graph: nothing is stored
       |- Triples: ${Model.Queryable.triples}; distinct subjects: ${Model.Queryable.subjects}; predicates: ${Model.Predicates.size}; classes: ${Model.Classes.size}
       |- Every count is exact and computed in closed form, including counts past 2^64
       |- License: ${Service.License}
       |
       |## Description
       |
       |${Service.Description}
       |
       |## Named graphs
       |
       |One per level, `${S2Graph.S2Prefix}{0..30}`. A pattern outside `GRAPH` reads the union of all 31; `GRAPH ?g` lists the level
       |graphs. The ten largest:
       |
       |$graphs
       |
       |## Classes
       |
       |$classes
       |
       |## Properties
       |
       |$properties
       |
       |## Namespaces
       |
       |$namespaces
       |
       |## What this server will not answer
       |
       |$refusal
       |
       |## Querying it
       |
       |A cell is named `${S2Graph.S2Prefix}13.5522341869704445952` and its geometry
       |`${S2Graph.GeometryPrefix}13.5522341869704445952`: the level in the IRI must be the cell's level and the id a plain unsigned
       |decimal, or the IRI names no cell. Bind a cell and ask for its neighbours, its ancestors or its descendants; a pattern bound only
       |by a class is answered too, and paged from any offset at the same cost.
       |""".stripMargin
