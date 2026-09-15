package org.renci.frink.qpf

import org.apache.jena.graph.Node
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.Syntax
import org.apache.jena.riot.out.NodeFmtLib
import org.apache.jena.sparql.core.Quad
import org.apache.jena.sparql.core.Var
import org.apache.jena.sparql.engine.binding.Binding
import org.apache.jena.sparql.syntax.ElementData
import org.apache.jena.sparql.syntax.ElementGroup
import org.renci.frink.Util.MultiSizedIterator
import org.renci.frink.Util.SizedIterator
import sttp.tapir.DecodeResult

import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** A table of bindings restricting a quad pattern, as in bindings-restricted Triple Pattern Fragments (brTPF): a quad is a match when the
  * pattern's variables take values compatible with at least one row. A row can leave a variable UNDEF, and can bind variables the pattern
  * doesn't have, which don't restrict it.
  */
final case class Bindings(variables: Seq[Var], rows: Seq[Binding]):
  import Bindings.*

  /** Patterns whose matches, together, are the matches of `pattern` compatible with some row: each row substituted into the pattern. A
    * pattern that another one subsumes is left out, since it can't add a match.
    */
  def restrict(pattern: Quad): Seq[Quad] =
    val substituted = rows.map { row =>
      def value(node: Node) = if node.isVariable() then Option(row.get(Var.alloc(node))).getOrElse(node) else node
      Quad.create(value(pattern.getGraph()), value(pattern.getSubject()), value(pattern.getPredicate()), value(pattern.getObject()))
    }.distinct
    substituted.filterNot(specific => substituted.exists(general => general != specific && subsumes(general, specific)))

  /** The matches of `pattern` compatible with some row, given how to find the matches of a pattern. A quad matching the patterns of several
    * rows is only at a position among the matches of the first; its positions among the matches of later rows are empty. That way positions
    * can be skipped without finding which matches repeat.
    */
  def matches(pattern: Quad, find: Quad => Either[String, SizedIterator[Quad]]): Either[String, Matches] =
    restrict(pattern)
      .foldLeft[Either[String, Vector[(Quad, SizedIterator[Quad])]]](Right(Vector.empty)) { (found, restriction) =>
        for
          earlier <- found
          quads <- find(restriction)
        yield if quads.size > 0 then earlier :+ (restriction -> quads) else earlier
      }
      .map { found =>
        val restrictions = found.map(_._1)
        val overlapping = restrictions.indices.map(i => restrictions.take(i).filter(overlaps(_, restrictions(i))))
        val positions = found.zip(overlapping).map { case ((_, quads), earlier) =>
          quads.map(quad => Option.unless(earlier.exists(subsumes(_, quad)))(quad))
        }
        Matches(MultiSizedIterator(positions), overlapping.forall(_.isEmpty))
      }

object Bindings:

  /** Most rows a request can have. Comunica sends at most 64 at a time unless configured otherwise. */
  val MaxRows = 1000

  /** SPARQL VALUES syntax without the VALUES keyword, which is how Comunica sends bindings */
  def parse(text: String): Try[Bindings] =
    Try(QueryFactory.create(s"SELECT * WHERE { VALUES $text\n}", Syntax.syntaxSPARQL_11)).flatMap { query =>
      val data = query.getQueryPattern() match
        case group: ElementGroup =>
          group.getElements().asScala.toSeq match
            case Seq(data: ElementData) => Some(data)
            case _                      => None
        case _ => None
      // the text could close the group pattern and add clauses after it
      val modified =
        query.hasLimit() || query.hasOffset() || query.hasOrderBy() || query.hasGroupBy() || query.hasHaving() || query.hasValues()
      data.filterNot(_ => modified).map(data => Bindings(data.getVars().asScala.toSeq, data.getRows().asScala.toSeq)) match
        case None => Failure(Exception("Bindings must be a single SPARQL VALUES block, without the VALUES keyword"))
        case Some(bindings) if bindings.rows.size > MaxRows =>
          Failure(Exception(s"Bindings can have at most $MaxRows rows, but there are ${bindings.rows.size}"))
        case Some(bindings) if bindings.rows.exists(row => row.vars().asScala.exists(row.get(_).isTripleTerm())) =>
          Failure(Exception("Bindings can't contain triple terms"))
        case Some(bindings) => Success(bindings)
    }

  /** Written in N-Triples form, since `parse` declares no prefixes to abbreviate terms with */
  def encode(bindings: Bindings): String =
    val variables = bindings.variables.map(variable => s"?${variable.getVarName()}").mkString("(", " ", ")")
    val rows = bindings.rows.map(row =>
      bindings.variables.map(variable => Option(row.get(variable)).map(NodeFmtLib.strNT).getOrElse("UNDEF")).mkString("(", " ", ")")
    )
    s"$variables { ${rows.mkString(" ")} }"

  def decode(text: String): DecodeResult[Bindings] = parse(text) match
    case Success(bindings) => DecodeResult.Value(bindings)
    case Failure(error)    => DecodeResult.Error(text, error)

  /** Whether every quad matching `specific` matches `general`. A quad without variables only matches itself. */
  def subsumes(general: Quad, specific: Quad): Boolean =
    val pairs = positions(general).zip(positions(specific))
    pairs.forall((g, s) => g.isVariable() || g == s) &&
    // a variable in several positions of `general` needs the same term in each of them
    pairs.filter(_._1.isVariable()).groupMap(_._1)(_._2).values.forall(_.distinct.size == 1)

  /** Whether two patterns might match the same quad */
  private def overlaps(a: Quad, b: Quad): Boolean =
    positions(a).zip(positions(b)).forall((x, y) => x.isVariable() || y.isVariable() || x == y)

  private def positions(quad: Quad): Seq[Node] =
    Seq(quad.getGraph(), quad.getSubject(), quad.getPredicate(), quad.getObject())
