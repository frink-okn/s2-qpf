package org.renci.frink.kgf

import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import org.apache.jena.sparql.core.Quad
import org.apache.jena.vocabulary.RDFS.Nodes as RDFS
import org.renci.frink.Util.MultiSizedIterator
import org.renci.frink.Util.SizedIterator
import org.renci.frink.qpf.QuadPatternFragment
import org.renci.frink.s2.S2Graph

/** The read operations: `/fragment`, `/count`, `/describe`, `/graphs` and `/terms`.
  *
  * Every answer is a `SizedIterator`, whose size is exact and whose `drop` skips positions arithmetically, so a page is a position and the
  * cost of a request does not depend on its offset (KGF doc 05 section 5.8).
  */
object Operations:

  /** Which graphs a request reads: the four forms of `g` (KGF doc 03 section 3.7, `docs/graph-semantics.md`) */
  final case class Scope(raw: Option[String], graph: Node, quadView: Boolean):

    /** Only the quad view adds a `g` column, and only it tags rows per graph in the RDF representations */
    def showsGraph: Boolean = quadView

  object Scope:
    val Union: Scope = Scope(None, QuadPatternFragment.UnionGraph, false)

    def parse(value: Option[String]): Either[Problem, Scope] =
      value.map(_.trim).filter(_.nonEmpty) match
        case None                => Right(Union)
        case Some(Term.QuadView) => Right(Scope(Some(Term.QuadView), Term.freshVariable, true))
        case Some(text) =>
          Term.parse(text).flatMap { node =>
            if node.isLiteral() || node.isVariable() then
              Left(Problem.malformed(s"g=$text is not a graph; name one with an IRI, with * for the quad view, or leave it out"))
            else if Term.isUnion(node) then Right(Scope(Some(text), QuadPatternFragment.UnionGraph, false))
            else Right(Scope(Some(text), node, false))
          }

  /** A parsed pattern request. `empty` marks a request whose filters can match nothing, which is an answer rather than an error. */
  final case class PatternRequest(
      subject: Option[Node],
      predicate: Option[Node],
      obj: Option[Node],
      scope: Scope,
      datatype: Option[String] = None,
      language: Option[String] = None,
      empty: Boolean = false,
      subjectVariable: Option[String] = None,
      predicateVariable: Option[String] = None,
      objectVariable: Option[String] = None,
      /** The predicate an object filter restricts the pattern to, which is not the pattern the client sent: a row still carries the
        * position the client left unbound, and the echoed pattern still shows it unbound.
        */
      restriction: Option[Node] = None
  ):

    def quad: Quad = Quad.create(
      scope.graph,
      subject.getOrElse(Term.freshVariable),
      predicate.orElse(restriction).getOrElse(Term.freshVariable),
      obj.getOrElse(Term.freshVariable)
    )

    /** The positions a row carries: the unbound ones, as SPARQL results name their variables */
    def vars: Seq[String] =
      Vector("s" -> subject, "p" -> predicate, "o" -> obj).collect { case (name, None) => name } ++
        Option.when(scope.showsGraph)("g")

    def patternJson: Json = Json.obj(
      "s" -> subject.map(node => Json.Str(Term.encode(node))).getOrElse(Json.Null),
      "p" -> predicate.map(node => Json.Str(Term.encode(node))).getOrElse(Json.Null),
      "o" -> obj.map(node => Json.Str(Term.encode(node))).getOrElse(Json.Null)
    )

    /** What a cursor and an ETag are bound to: the request, spelled canonically, so that a CURIE and an IRI share a page */
    def canonical: String =
      val positions = Vector(subject, predicate, obj).map(_.map(Term.encode).getOrElse(""))
      (positions ++ Vector(scope.raw.getOrElse(""), datatype.getOrElse(""), language.getOrElse(""))).mkString("\n")

  object PatternRequest:

    /** Each predicate here has one object datatype, so a datatype restriction is a predicate restriction, answered exactly rather than by a
      * filtered scan. No literal of this graph carries a language tag, so `o.lang` matches nothing, also exactly.
      */
    private val datatypePredicates: Map[String, Node] = Map(
      Model.XSDString -> RDFS.label,
      Model.XSDInteger -> S2Graph.CellIDNode,
      Model.XSDDouble -> S2Graph.HasMetricAreaNode,
      Model.WKTLiteral -> S2Graph.AsWKTNode
    )

    def parse(
        subject: Option[String],
        predicate: Option[String],
        obj: Option[String],
        graph: Option[String],
        datatype: Option[String],
        language: Option[String]
    ): Either[Problem, PatternRequest] =
      for
        scope <- Scope.parse(graph)
        s <- parsePosition("s", subject, literalAllowed = false)
        p <- parsePosition("p", predicate, literalAllowed = false)
        o <- parsePosition("o", obj, literalAllowed = true)
        dt <- datatype.map(_.trim).filter(_.nonEmpty).map(Term.datatype(_).map(Some(_))).getOrElse(Right(None))
      yield restrict(
        PatternRequest(
          s._1,
          p._1,
          o._1,
          scope,
          dt,
          language.map(_.trim).filter(_.nonEmpty),
          subjectVariable = s._2,
          predicateVariable = p._2,
          objectVariable = o._2
        )
      )

    /** A bound term, or the name of the variable the client wrote, which a bindings row can fill */
    private def parsePosition(
        name: String,
        value: Option[String],
        literalAllowed: Boolean
    ): Either[Problem, (Option[Node], Option[String])] =
      value.map(_.trim).filter(_.nonEmpty) match
        case None => Right(None -> None)
        case Some(text) =>
          Term.parse(text).flatMap { node =>
            if node.isLiteral() && !literalAllowed then
              Left(Problem.malformed(s"$name=$text is a literal, which only the object position can hold"))
            else if node.isVariable() then Right(None -> Some(node.getName()))
            else Right(Some(node) -> None)
          }

    /** The object filters, resolved into the pattern */
    private def restrict(request: PatternRequest): PatternRequest =
      val unanswerable = request.copy(empty = true)
      if request.language.isDefined then unanswerable
      else
        request.datatype match
          case None => request
          case Some(iri) =>
            datatypePredicates.get(iri) match
              case None => unanswerable
              case Some(predicate) =>
                val objectFits = request.obj.forall(node => node.isLiteral() && node.getLiteralDatatypeURI() == iri)
                if !objectFits then unanswerable
                else if request.predicate.forall(_ == predicate) then request.copy(restriction = Some(predicate))
                else unanswerable

  /** A page of an answer. A row carries the index of the binding row it joins with where the request had bindings. */
  final case class Page(rows: Seq[(Option[Int], Quad)], cardinality: BigInt, from: BigInt, limit: Int):
    def complete: Boolean = from + rows.size >= cardinality

    def nextPosition: BigInt = from + rows.size

  private val Nothing: SizedIterator[Quad] = SizedIterator.empty

  /** The quads matching a pattern, or the reason this dataset cannot answer it within the cost contract */
  def quads(request: PatternRequest): Either[Problem, SizedIterator[Quad]] =
    if request.empty then Right(Nothing)
    else S2Graph.quads(request.quad).left.map(Problem.refused)

  def page(found: SizedIterator[Quad], from: BigInt, limit: Int): Page =
    Page(found.drop(from).iterator.take(limit).map(None -> _).toSeq, found.size, from, limit)

  /** `/describe`: every triple with the resource as subject, as object, or both, which is two pattern enumerations */
  def describe(resource: Node, direction: String, scope: Scope): Either[Problem, SizedIterator[Quad]] =
    val outgoing = PatternRequest(Some(resource), None, None, scope)
    val incoming = PatternRequest(None, None, Some(resource), scope)
    direction match
      case "out"  => quads(outgoing)
      case "in"   => quads(incoming)
      case "both" => for first <- quads(outgoing); second <- quads(incoming) yield MultiSizedIterator(Vector(first, second))
      case other =>
        Left(Problem.malformed(s"direction=$other is not a direction; it is one of out, in, both"))

  /** The matches of each binding row in turn, each row's quads carrying that row's index.
    *
    * One row per (input row, quad): KGF doc 03 section 3.4.2 gives every result row a `binding` column, so a quad that several rows match
    * is several rows and nothing is deduplicated. That is what keeps the cardinality exact — it is the sum of the per-row counts — where
    * the QPF route, whose answer is one set of quads with one total, has to drop repeats and report an upper bound.
    */
  def restricted(request: PatternRequest, rows: Seq[Map[String, Node]]): Either[Problem, SizedIterator[(Int, Quad)]] =
    val answers = rows.zipWithIndex.map { (row, index) =>
      index -> quads(substitute(request, row))
    }
    val refused = answers.collect { case (index, Left(problem)) => index -> problem }
    if refused.nonEmpty then
      val indices = refused.map(_._1).mkString(", ")
      Left(
        Problem.refused(
          s"${refused.head._2.detail} Rows that cannot be answered: $indices. Drop them and send the rest, which this dataset answers."
        )
      )
    else
      Right(
        MultiSizedIterator(
          answers.toVector.collect { case (index, Right(found)) => found.map(quad => (index, quad)) }
        )
      )

  def counts(request: PatternRequest, rows: Seq[Map[String, Node]]): Either[Problem, Seq[(Int, BigInt)]] =
    val answers = rows.zipWithIndex.map((row, index) => index -> quads(substitute(request, row)))
    val refused = answers.collect { case (index, Left(problem)) => index -> problem }
    if refused.nonEmpty then
      Left(Problem.refused(s"${refused.head._2.detail} Rows that cannot be answered: ${refused.map(_._1).mkString(", ")}."))
    else Right(answers.collect { case (index, Right(found)) => index -> found.size })

  /** A pattern with one row's terms in place of the variables it names. A variable the row leaves UNDEF stays unbound, and a variable the
    * pattern does not have restricts nothing.
    */
  private def substitute(request: PatternRequest, row: Map[String, Node]): PatternRequest =
    def bound(position: Option[Node], name: Option[String]): Option[Node] = position.orElse(name.flatMap(row.get))
    request.copy(
      subject = bound(request.subject, request.subjectVariable),
      predicate = bound(request.predicate, request.predicateVariable),
      obj = bound(request.obj, request.objectVariable)
    )

  /** The graphs this dataset has: one per level, each with its exact count. The unnamed graph is never listed, because every quad here
    * carries a level (KGF doc 03 section 3.7).
    */
  def graphs: SizedIterator[(Node, BigInt)] =
    SizedIterator.fromSeq(Model.Levels.map(level => S2Graph.Graphs(level) -> Model.levelTriples(level)))
