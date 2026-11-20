package org.renci.frink.kgf

import org.apache.jena.graph.Node
import org.apache.jena.vocabulary.RDF.Nodes as RDF
import org.renci.frink.s2.S2Graph

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/** `/schema`: the bounded drill-down between the summary card and the whole VoID document (KGF doc 03 section 3.4.10).
  *
  * One partition node and at most one page of its immediate children, never a child's own breakdown — the one-edge rule, which is what
  * makes `limit` a bound on the whole response shape rather than on its outermost list. The two flat projections page the same tree
  * pre-sorted by triples, computed here rather than read from a build-time artifact; the row count is bounded by the vocabulary and the 31
  * levels.
  */
object Schema:

  final case class Request(
      view: Model.View,
      classTerm: Option[Node],
      predicate: Option[Node],
      datatype: Option[Node],
      children: Option[String],
      projection: Option[String],
      limit: Int,
      cursor: Option[String]
  ):
    def canonical: String =
      Vector(
        view.name,
        classTerm.map(Term.encode).getOrElse(""),
        predicate.map(Term.encode).getOrElse(""),
        datatype.map(Term.encode).getOrElse(""),
        children.getOrElse(""),
        projection.getOrElse("")
      ).mkString("\n")

  private def encode(value: String): String = URLEncoder.encode(value, StandardCharsets.UTF_8)

  private def link(view: Model.View, parameters: (String, String)*): Json =
    val query = (parameters.toVector :+ ("view" -> view.name)).map((name, value) => s"$name=${encode(value)}").mkString("&")
    Json.Str(s"?$query")

  private def counts(entries: (String, Option[BigInt])*): Json =
    Json.Obj(entries.toVector.collect { case (name, Some(value)) => name -> Json.Num(value) })

  /** The selector a request names, echoed complete even when the node it names is absent */
  private def selector(request: Request): Json =
    val kind =
      if request.datatype.isDefined then "datatype"
      else if request.predicate.isDefined then "property"
      else if request.classTerm.isDefined then "class"
      else "dataset"
    Json.Obj(
      Vector("kind" -> Json.Str(kind)) ++
        request.classTerm.map(term => "class" -> Json.term(term)) ++
        request.predicate.map(term => "predicate" -> Json.term(term)) ++
        request.datatype.map(term => "datatype" -> Json.term(term))
    )

  /** Which child collections a selector shape allows (KGF doc 03 section 3.4.10) */
  private def allowed(request: Request): Seq[String] =
    if request.datatype.isDefined then Vector("languages")
    else if request.predicate.isDefined then Vector("object-classes", "datatypes")
    else if request.classTerm.isDefined then Vector("properties")
    else Vector("classes", "properties")

  def answer(request: Request): Either[Problem, Json] =
    request.projection match
      case Some("class-relations")  => Right(flat(request, relationRows(request)))
      case Some("class-properties") => Right(flat(request, classPropertyRows(request)))
      case Some(other) =>
        Left(Problem.malformed(s"projection=$other is not a projection; it is class-relations or class-properties"))
      case None => node(request)

  private def node(request: Request): Either[Problem, Json] =
    request.children match
      case Some(collection) if !allowed(request).contains(collection) =>
        Left(
          Problem.malformed(
            s"children=$collection is not a collection of the selected node; it offers ${allowed(request).mkString(", ")}"
          )
        )
      case _ =>
        val view = request.view
        val selected: Option[Json] = (request.classTerm, request.predicate, request.datatype) match
          case (None, None, None) =>
            Some(
              Json.obj(
                "kind" -> Json.Str("dataset"),
                "counts" -> counts(
                  "triples" -> Some(view.triples),
                  "distinct_subjects" -> Some(view.subjects),
                  "properties" -> Some(BigInt(view.properties.size)),
                  "classes" -> Some(BigInt(view.classes.size))
                ),
                "links" -> Json.obj(
                  "classes" -> link(view, "children" -> "classes"),
                  "properties" -> link(view, "children" -> "properties"),
                  "class-properties" -> link(view, "projection" -> "class-properties"),
                  "class-relations" -> link(view, "projection" -> "class-relations")
                )
              )
            )
          case (Some(classTerm), None, None) => view.classPartition(classTerm).map(classNode(view, _))
          case (classTerm, Some(predicate), None) =>
            property(view, classTerm, predicate).map(propertyNode(view, classTerm, _))
          case (classTerm, predicate, Some(datatype)) =>
            predicate
              .flatMap(term => property(view, classTerm, term))
              .filter(_.datatype.contains(datatype.getURI()))
              .map(found => datatypeNode(view, classTerm, found, datatype))
          case _ => None
        val items = request.children.map(collection => page(request, collection))
        Right(
          Json.Obj(
            Vector(
              "dataset" -> Json.Str(Service.DatasetId),
              "version" -> Json.Str(Service.Version),
              "view" -> Json.Str(view.name),
              "selector" -> selector(request),
              "node" -> selected.getOrElse(Json.Null)
            ) ++ items.map(_._1).getOrElse(Vector.empty) ++ items.map(_._2).getOrElse(Completeness.Complete).fields
          )
        )

  private def property(view: Model.View, classTerm: Option[Node], predicate: Node): Option[Model.Property] =
    classTerm.map(term => view.propertyOf(term, predicate)).getOrElse(view.property(predicate))

  private def classNode(view: Model.View, partition: Model.ClassPartition): Json =
    Json.obj(
      "kind" -> Json.Str("class"),
      "term" -> Json.term(partition.term),
      "counts" -> counts("entities" -> Some(partition.entities), "triples" -> Some(partition.triples)),
      "links" -> Json.obj(
        "self" -> link(view, "class" -> Term.encode(partition.term)),
        "properties" -> link(view, "class" -> Term.encode(partition.term), "children" -> "properties")
      )
    )

  private def propertyNode(view: Model.View, classTerm: Option[Node], found: Model.Property): Json =
    val scope = classTerm.toVector.map(term => "class" -> Term.encode(term))
    Json.obj(
      "kind" -> Json.Str("property"),
      "term" -> Json.term(found.predicate),
      "counts" -> counts(
        "triples" -> Some(found.triples),
        "distinct_subjects" -> found.subjects,
        "distinct_objects" -> found.objects
      ),
      "links" -> Json.Obj(
        Vector(
          "self" -> link(view, (scope :+ ("predicate" -> Term.encode(found.predicate)))*),
          "object-classes" -> link(view, (scope ++ Vector("predicate" -> Term.encode(found.predicate), "children" -> "object-classes"))*),
          "datatypes" -> link(view, (scope ++ Vector("predicate" -> Term.encode(found.predicate), "children" -> "datatypes"))*)
        )
      )
    )

  private def datatypeNode(view: Model.View, classTerm: Option[Node], found: Model.Property, datatype: Node): Json =
    val scope = classTerm.toVector.map(term => "class" -> Term.encode(term))
    Json.obj(
      "kind" -> Json.Str("datatype"),
      "term" -> Json.term(datatype),
      "counts" -> counts("triples" -> Some(found.triples)),
      "links" -> Json.obj(
        "languages" -> link(
          view,
          (scope ++ Vector("predicate" -> Term.encode(found.predicate), "datatype" -> Term.encode(datatype), "children" -> "languages"))*
        )
      )
    )

  /** One page of the selected node's one child collection */
  private def page(request: Request, collection: String): (Seq[(String, Json)], Completeness) =
    val view = request.view
    val rows: Seq[Json] = collection match
      case "classes" => view.classes.map(classNode(view, _))
      case "properties" =>
        val properties = request.classTerm match
          case Some(term) => view.classPartition(term).map(_.properties).getOrElse(Seq.empty)
          case None       => view.properties
        properties.map(propertyNode(view, request.classTerm, _))
      case "object-classes" => objectClasses(view, request.predicate)
      case "datatypes" =>
        request.predicate
          .flatMap(predicate => property(view, request.classTerm, predicate))
          .toVector
          .flatMap(found => found.datatype.map(datatype => datatypeItem(found, datatype)))
      // no literal of this graph carries a language tag, so this collection is always empty, which is schema information
      case _ => Seq.empty
    val (items, completeness) = slice(request, rows)
    (
      Vector(
        "collection" -> Json.obj(
          "kind" -> Json.Str(collection),
          "returned" -> Json.int(items.size),
          "order" -> Json.Str("published")
        ),
        "items" -> Json.Arr(items)
      ),
      completeness
    )

  private def datatypeItem(found: Model.Property, datatype: String): Json =
    Json.obj(
      "kind" -> Json.Str("datatype"),
      "term" -> Json.term(org.apache.jena.graph.NodeFactory.createURI(datatype)),
      "counts" -> counts("triples" -> Some(found.triples))
    )

  /** The classes the objects of a predicate belong to.
    *
    * Taken from the class relations whose subject class is `kwg-ont:S2Cell`, which every subject of these predicates belongs to exactly
    * once, so the rows are the predicate's own object partitions rather than a sum over the four ways a cell is typed. `rdf:type`'s objects
    * are classes, which this server states nothing about, so its targets are untyped and this collection is empty for it.
    */
  private def objectClasses(view: Model.View, predicate: Option[Node]): Seq[Json] =
    predicate.toVector.flatMap { term =>
      view.relations
        .filter(relation => relation.predicate == term && relation.subjectClass == S2Graph.S2CellClass)
        .groupBy(_.objectClass)
        .toVector
        .map((objectClass, group) => objectClass -> group.map(_.triples).sum)
        .sortBy((objectClass, triples) => (-triples, objectClass.getURI()))
        .map((objectClass, triples) =>
          Json.obj(
            "kind" -> Json.Str("object-class"),
            "term" -> Json.term(objectClass),
            "counts" -> counts("triples" -> Some(triples))
          )
        )
    }

  private def relationRows(request: Request): Seq[Json] =
    request.view.relations
      .filter(relation => request.classTerm.forall(_ == relation.subjectClass) && request.predicate.forall(_ == relation.predicate))
      .map(relation =>
        Json.obj(
          "subject_class" -> Json.term(relation.subjectClass),
          "predicate" -> Json.term(relation.predicate),
          "object_class" -> Json.term(relation.objectClass),
          "triples" -> Json.Num(relation.triples)
        )
      )

  private def classPropertyRows(request: Request): Seq[Json] =
    request.view.classes
      .filter(partition => request.classTerm.forall(_ == partition.term))
      .flatMap(partition => partition.properties.map(partition.term -> _))
      .filter((_, property) => request.predicate.forall(_ == property.predicate))
      .sortBy((classTerm, property) => (-property.triples, classTerm.getURI(), property.predicate.getURI()))
      .map((classTerm, property) =>
        Json.Obj(
          Vector(
            "class" -> Json.term(classTerm),
            "predicate" -> Json.term(property.predicate),
            "triples" -> Json.Num(property.triples)
          ) ++ property.subjects.map(count => "distinct_subjects" -> Json.Num(count)) ++
            property.objects.map(count => "distinct_objects" -> Json.Num(count))
        )
      )

  private def flat(request: Request, rows: Seq[Json]): Json =
    val (items, completeness) = slice(request, rows)
    val tieBreak =
      if request.projection.contains("class-relations") then Vector("subject-class", "predicate", "object-class")
      else Vector("class", "predicate")
    Json.Obj(
      Vector(
        "dataset" -> Json.Str(Service.DatasetId),
        "version" -> Json.Str(Service.Version),
        "view" -> Json.Str(request.view.name),
        "projection" -> Json.Str(request.projection.getOrElse("")),
        "filters" -> Json.Obj(
          request.classTerm.toVector.map(term => "class" -> Json.term(term)) ++
            request.predicate.toVector.map(term => "predicate" -> Json.term(term))
        ),
        "order" -> Json.obj(
          "by" -> Json.Str("triples"),
          "direction" -> Json.Str("descending"),
          "tie_break" -> Json.Arr(tieBreak.map(Json.Str(_)))
        ),
        "items" -> Json.Arr(items)
      ) ++ completeness.fields
    )

  /** One page of a collection, with the cursor that resumes it */
  private def slice(request: Request, rows: Seq[Json]): (Seq[Json], Completeness) =
    val from = request.cursor.flatMap(token => Cursor.decode(token, "schema", request.canonical).toOption).getOrElse(BigInt(0))
    val start = if from > rows.size then rows.size else from.toInt
    val items = rows.slice(start, start + request.limit)
    if start + items.size >= rows.size then (items, Completeness.Complete)
    else (items, Completeness.pageLimit(Cursor.encode("schema", request.canonical, BigInt(start + items.size))))
