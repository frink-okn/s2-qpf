package org.renci.frink.qpf

import org.apache.jena.graph.Node
import org.apache.jena.shared.PrefixMapping
import org.renci.frink.kgf.Html
import org.renci.frink.qpf.Types.Term
import scalatags.Text.all.*
import scalatags.Text.tags2

/** The HTML view of a Quad Pattern Fragment: the search form a browser submits, and the page's quads.
  *
  * The page is built in `kgf.Html.page`, the shell and stylesheet every resource of this server shares, so that the TPF route looks like
  * the rest of the site rather than like a second one.
  */
object WebUI:

  def toHTML(qpf: QuadPatternFragment): String =
    val metadata = qpf.metadata
    val params = metadata.params
    val prefixes = qpf.prefixMapping
    val firstItem = metadata.skip + 1
    val lastItem = firstItem + qpf.data.size - 1

    def patternTerm(term: Option[Term], variable: String) = term match
      case Some(Term.IRI(iri))         => s"<$iri>"
      case Some(literal: Term.Literal) => Term.encode(literal)
      case Some(Term.Variable(name))   => s"?$name"
      case None                        => variable
    // variables are shown, since bindings refer to them
    def fieldValue(term: Option[Term]) = term.map(Term.encode).getOrElse("")
    val pattern = s"{ ${patternTerm(params.s, "?s")} ${patternTerm(params.p, "?p")} ${patternTerm(params.o, "?o")} " +
      s"${patternTerm(params.g, "?g")} }"

    val navigation = tags2.nav(
      Option.when(metadata.page != 1)(a(href := metadata.fragmentURL, rel := "first")("first")),
      metadata.previousPage.map(uri => a(href := uri, rel := "prev")("previous")),
      metadata.nextPage.map(uri => a(href := uri, rel := "next")("next"))
    )

    val search = form(action := "?", method := "GET")(
      h2("Search by quad pattern"),
      Html.field("subject", "subject", input(id := "subject", name := "subject", value := fieldValue(params.s))),
      Html.field("predicate", "predicate", input(id := "predicate", name := "predicate", value := fieldValue(params.p))),
      Html.field("object", "object", input(id := "object", name := "object", value := fieldValue(params.o))),
      Html.field("graph", "graph", input(id := "graph", name := "graph", value := fieldValue(params.g))),
      Html.field(
        "values",
        "values",
        // one element, since the field is a two-column grid and a fragment's children would each take a cell of their own
        div(
          textarea(
            id := "values",
            name := "values",
            placeholder := "(?s) { (<http://stko-kwg.geog.ucsb.edu/lod/resource/s2.level13.5525166171478818816>) }"
          )(params.values.map(Bindings.encode).getOrElse("")),
          div(cls := "muted")("bindings of the pattern's variables, in SPARQL VALUES syntax")
        )
      ),
      Html.field("submit", "", input(`type` := "submit", value := "Search"))
    )

    val summary =
      if metadata.totalItems == 0 then p(cls := "muted")("No matches for this pattern.")
      else if metadata.exactTotal then
        p(cls := "muted")(f"Showing $firstItem%,d to $lastItem%,d of ${metadata.totalItems}%,d, ${metadata.itemsPerPage}%,d per page.")
      else
        p(cls := "muted")(
          f"Showing ${qpf.data.size}%,d from positions $firstItem%,d to ${(metadata.skip + metadata.itemsPerPage).min(metadata.totalItems)}%,d " +
            f"of at most ${metadata.totalItems}%,d, since a quad matching several bindings is shown only at its first position."
        )

    // abbreviated with the dataset's published prefix map, as the KGF pages abbreviate: one term reads one way across the server
    def show(term: Node) = if term.isURI() then Html.abbreviate(term.getURI()) else term.toString(prefixes)
    def cell(term: Node, parameter: String, requested: String) =
      td(a(href := metadata.endpoint.withParam(parameter, requested).toString, title := term.toString)(show(term)))

    val results = tag("table")(
      thead(tr(th("subject"), th("predicate"), th("object"), th("graph"))),
      tbody(
        qpf.data.map(quad =>
          tr(
            cell(quad.getSubject(), "subject", quad.getSubject().toString()),
            cell(quad.getPredicate(), "predicate", quad.getPredicate().toString()),
            cell(quad.getObject(), "object", Term.encode(Term.fromNode(quad.getObject()))),
            cell(quad.getGraph(), "graph", quad.getGraph().toString())
          )
        )
      )
    )

    Html.page(
      "tpf",
      search,
      h2("Matches"),
      p(
        span(cls := "pattern")(pattern),
        params.values.map(bindings => frag(" ", span(cls := "pattern")(s"VALUES ${Bindings.encode(bindings)}")))
      ),
      summary,
      navigation,
      results,
      navigation
    )
