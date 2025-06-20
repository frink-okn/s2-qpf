package org.renci.frink.qpf

import org.apache.jena.graph.Node
import org.apache.jena.sparql.core.DatasetGraph
import org.apache.jena.sparql.vocabulary.FOAF
import org.renci.frink.qpf.Types.VariableOrIRI
import scalatags.Text.all.*
import scalatags.Text.tags2

import scala.jdk.CollectionConverters.*

object WebUI:

  def toHTML(qpf: QuadPatternFragment): String =
    val m = qpf.metadata
    val params = m.params
    val pm = qpf.prefixMapping
    val firstItem = m.skip + 1
    val lastItem = firstItem + qpf.data.size - 1
    val first = (if m.page == 1 then None else Some(m.currentQPF.withParam("page", None))).map(uri =>
      a(href := uri.toString, rel := "first")("first")
    )
    val prev = m.previousQPFPage.map(uri => a(href := uri.toString, rel := "prev")("previous"))
    val next = m.nextQPFPage.map(uri => a(href := uri.toString, rel := "next")("next"))
    val nav = List(first, prev, next).flatten
    val subj = params.s.collect { case iri: VariableOrIRI.IRI => iri }.map(i => s"<${i.value}>").getOrElse("?s")
    val pred = params.p.collect { case iri: VariableOrIRI.IRI => iri }.map(i => s"<${i.value}>").getOrElse("?p")
    val obj = params.o.collect { case iri: VariableOrIRI.IRI => iri }.map(i => s"<${i.value}>").getOrElse("?o")
    val graph = params.g.collect { case iri: VariableOrIRI.IRI => iri }.map(i => s"<${i.value}>").getOrElse("?g")
    "<!DOCTYPE html>" +
      html(
        head(
          meta(charset := "utf-8"),
          tags2.title("Linked S2 Geometry"),
          link(rel := "stylesheet", href := "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.classless.min.css")
        ),
        body(
          header(
            h1("S2 Geometry Quad Pattern Fragments server"),
            tags2.aside("More about ", a(href := "http://s2geometry.io")("S2 Geometry")),
            tags2.aside("More about ", a(href := "https://linkeddatafragments.org")("Linked Data Fragments"))
          ),
          tags2.main(
            div(
              h2(a(href := m.endpoint.toString)("S2 Dataset")),
              form(action := "?", method := "GET")(
                fieldset(
                  div(
                    legend(h3("Search S2 by quad pattern")),
                    div(
                      label(`for` := "subject")("subject"),
                      input(
                        id := "subject",
                        name := "subject",
                        value := params.s.collect { case i: VariableOrIRI.IRI => i }.map(VariableOrIRI.encode).getOrElse("")
                      )
                    ),
                    div(
                      label(`for` := "predicate")("predicate"),
                      input(
                        id := "predicate",
                        name := "predicate",
                        value := params.p.collect { case i: VariableOrIRI.IRI => i }.map(VariableOrIRI.encode).getOrElse("")
                      )
                    ),
                    div(
                      label(`for` := "object")("object"),
                      input(
                        id := "object",
                        name := "object",
                        value := params.o.collect { case i: VariableOrIRI.IRI => i }.map(VariableOrIRI.encode).getOrElse("")
                      )
                    ),
                    div(
                      label(`for` := "graph")("graph"),
                      input(
                        id := "graph",
                        name := "graph",
                        value := params.g.collect { case i: VariableOrIRI.IRI => i }.map(VariableOrIRI.encode).getOrElse("")
                      )
                    )
                  )
                ),
                p(input(`type` := "submit", value := "Search"))
              )
            ),
            h3("Matches in S2 for ", em(s"{ $subj $pred $obj $graph }")),
            div(
              if m.totalItems > 0 then
                p(
                  s"Showing items $firstItem to $lastItem of ",
                  span(f"${m.totalItems}%,d"),
                  " with ",
                  span(m.itemsPerPage.toString()),
                  " items per page."
                )
              else p("There are no matches for this pattern.")
            ),
            tags2.nav(nav),
            table(
              thead(
                tr(
                  th("Subject"),
                  th("Predicate"),
                  th("Object"),
                  th("Graph")
                )
              ),
              tbody(
                qpf.data.map(q =>
                  tr(
                    td(
                      a(
                        href := m.endpoint.withParam("subject", q.getSubject().toString()).toString,
                        tags2.abbr(title := q.getSubject().toString())(q.getSubject().toString(pm))
                      )
                    ),
                    td(
                      a(
                        href := m.endpoint.withParam("predicate", q.getPredicate().toString()).toString,
                        tags2.abbr(title := q.getPredicate().toString())(q.getPredicate().toString(pm))
                      )
                    ),
                    td(
                      a(
                        href := m.endpoint.withParam("object", q.getObject().toString()).toString,
                        tags2.abbr(title := q.getObject().toString())(q.getObject().toString(pm))
                      )
                    ),
                    td(
                      a(
                        href := m.endpoint.withParam("graph", q.getGraph().toString()).toString,
                        tags2.abbr(title := q.getGraph().toString())(q.getGraph().toString(pm))
                      )
                    )
                  )
                )
              )
            ),
            tags2.nav(nav)
          ),
          footer(
            "Part of the ",
            a(href := "https://frink.renci.org")("FRINK project"),
            ", funded by ",
            a(href := "https://www.nsf.gov/awardsearch/showAward?AWD_ID=2333810")("NSF award 2333810"),
            " to ",
            a(href := "https://renci.org")("RENCI", ". "),
            "Source on ",
            a(href := "https://github.com/frink-okn/s2-qpf")("GitHub"),
            "."
          )
        )
      ).toString
