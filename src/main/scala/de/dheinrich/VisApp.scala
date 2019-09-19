package de.dheinrich

import org.scalajs.dom
import scalatags.JsDom.all._
import scalatags.rx.all._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.js.JSON
import rx._

import rx.Ctx.Owner.Unsafe._
import scalatags.JsDom.all

object VisApp extends App {
  println("starting credit viz!")

  val main = dom.document.getElementsByTagName("main")(0)

  val json = JSON.parse("""
    {
        "$schema": "https://vega.github.io/schema/vega-lite/v4.json",
        "data": {"url": "data/movies.json"},
        "mark": "bar",
        "encoding": {
          "x": {
            "bin": true,
            "field": "IMDB_Rating",
            "type": "quantitative"
          },
          "y": {
            "aggregate": "count",
            "type": "quantitative"
          }
        }
      }
      
    """)

  val darlehen = Var(List[Var[Darlehen]]())

  val darlehenForms = Rx {
    darlehen().zipWithIndex.map {
      case (d, i) => {
        def remove() = darlehen() = darlehen.now.zipWithIndex.filter(_._2 != i).map(_._1)

        def fieldInput(
            name: String,
            extractor: Darlehen => Any,
            builder: (Darlehen, String) => Darlehen
        ) = {
          def changeValue(): Unit = {
            d() = builder(d.now, valueInput.value)
          }

          lazy val valueInput = input(
            id := s"$name$i",
            `class` := "form-control",
            onkeyup := changeValue _,
            value := Rx(extractor(d()).toString())
          ).render

          div(
            `class` := "form-group col",
            label(`for` := s"$name$i")(name),
            valueInput
          )
        }

        div(
          `class` := "card",
          div(
            `class` := "card-body",
            form(
              div(
                `class` := "form-row",
                fieldInput("Summe", _.summe, (o, s) => o.copy(summe = BigDecimal(s))),
                fieldInput("Rate", _.rate, (o, s) => o.copy(rate = BigDecimal(s))),
                fieldInput("Zins", _.zins, (o, s) => o.copy(zins = BigDecimal(s)))
              ),
              div(
                `class` := "form-row",
                fieldInput("Laufzeit", _.laufzeit, (o, s) => o.copy(laufzeit = s.toInt)),
                fieldInput(
                  "Sondertilgung",
                  _.sonderTilgungPerYearPercent,
                  (o, s) => o.copy(sonderTilgungPerYearPercent = BigDecimal(s))
                ),
                fieldInput(
                  "TilgunsfreieMonate",
                  _.tilgungsfreieMonate,
                  (o, s) => o.copy(tilgungsfreieMonate = s.toInt)
                )
              )
            ),
            button(onclick := remove _, `class` := "btn btn-danger")(
              span(`class` := "oi oi-trash")
            )
          )
        ).render
      }
    }
  }

  def newDarlehen = darlehen() = darlehen.now :+ Var(Darlehen(100000, 0.01, 1000, 15, 0.05))

  val gesammtSumme = Rx {
    darlehen().map(_().summe).sum
  }

  val outcome = Rx {
    val kredite = darlehen().map(_())
    println(s"${kredite.size} kredite")
    val out = Darlehen.simulate(Tilgung.fix(0), kredite: _*)
    println(s"${out.size} out")
    out
  }

  val outcomeRows = Rx {
    outcome().map(
      st =>
        tr(
          td(st.year.toString()),
          td(st.month.toString()),
          td(f"${st.schuld.doubleValue()}%.2f €"),
          td(f"${st.zinsenPayed.doubleValue()}%.2f €")
        ).render
    )
  }

  main.appendChild(
    div(
      `class` := "container-fluid",
      div(
        `class` := "row",
        div(
          `class` := "col-4",
          darlehenForms,
          div(Rx(s"gesammt summe: ${gesammtSumme()}")),
          button(onclick := newDarlehen _, `class` := "btn btn-primary")("Neues Darlehen")
        ),
        div(
          `class` := "col-8",
          table(
            `class` := "table",
            thead(
              tr(
                th(attr("scope") := "col")("Jahr"),
                th(attr("scope") := "col")("Monat"),
                th(attr("scope") := "col")("Restschuld"),
                th(attr("scope") := "col")("Geleisteter Zins")
              )
            ),
            tbody(outcomeRows)
          )
        )
      )
    ).render
  )

  // main.appendChild(div(id := "vis")("Hello World2").render)

  // VegaScope.vegaEmbed("#vis", json)
}

@js.native
@JSGlobalScope
object VegaScope extends js.Object {
  def vegaEmbed(selector: String, spec: js.Dynamic): Unit = js.native
}
