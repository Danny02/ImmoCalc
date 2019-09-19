package de.dheinrich
import scala.annotation.tailrec

object Calc extends App {
  // Kosten
  val extras = 0
  val preis  = 400000 - extras

  // haben
  val eigenkapital = 100000
  val kfwSumme     = 50000

  //
  // nebenkosten
  val steuer  = BigDecimal(0.035)
  val markler = BigDecimal(0.0476)
  val notar   = BigDecimal(0.02)

  val nebenkosten = preis * (steuer + markler + notar)
  println(s"nebenkosten sind $nebenkosten")

  //
  // eigen anteil
  val eigenanteil = (eigenkapital - nebenkosten) / preis
  println(s"eigenanteil ist $eigenanteil")

  val darlehen = preis - (eigenkapital - extras - nebenkosten)
  println(s"zu leihen $darlehen")

  def eigenkaptialFuerAnteil(anteil: Double) =
    anteil * preis + nebenkosten + extras

  for (anteil <- Seq(10, 20, 30, 40)) {
    println(
      s"Anteil von $anteil% sind ${eigenkaptialFuerAnteil(anteil / 100.0)}"
    )
  }

  def printOutcome(outcome: List[Darlehen.State]) = {
    outcome.foreach { os =>
      val msg =
        if (os.schuld == 0)
          s"Nach ${os.year - os.darlehen.ausgesetzInErstenJahren} Jahren und ${os.month} Monaten abgezahlt."
        else {
          val (year, month) = os.payedOffAt()
          s"Würde nach $year Jahren und $month Monaten abgezahlt werde."
        }

      println(
        f"${os.schuld.toDouble}%.2f Restschuld, ${os.zinsenPayed.toDouble}%.2f Zinsen gezahlt. $msg%s"
      )
    }
    val zTotal  = outcome.map(_.zinsenPayed).sum
    val rTotal  = outcome.map(_.schuld).sum
    val zPayOff = outcome.map(_.payedOffState.zinsenPayed).sum
    println("###TOTAL###")
    println(
      f"${rTotal.toDouble}%.2f Restschuld, ${zTotal.toDouble}%.2f Zinsen gezahlt. (Für vollständigen Tilgung würden noch ${(zPayOff - zTotal).toDouble}%.2f Zinsen gezahlt werden)"
    )
  }

  //
  // zins
  val pb15 = Darlehen(darlehen - kfwSumme, 0.007, 1500, 15, 0.05)
  val pb20 = Darlehen(darlehen - kfwSumme, 0.0115, 1313.25, 20, 0.05)
  val kfw  = Darlehen(kfwSumme, 0.0075, 235.36, 10, 0, tilgungsfreieMonate = 12)

  val partner1 = Tilgung
    .fix(1250)
    // .bonus(1500, 3)
    .elternzeit(21, 7)
  // .elternzeit(45, 7)

  val partner2 = Tilgung
    .fix(1250)
    // .bonus(1300, 12)
    .elternzeit(16, 7)
  // .elternzeit(40, 7)
  // .ausfall(12 * 5, 2 * 12)

  val totalT = (partner1 combine partner2)
    .withInflationPA(0.01)
    .extraKostenNach(16, 1000)
  // .extraKostenNach(40, 1000)

  // val restD = Darlehen(60000, 0.05, 1700, 5, 0.05, ausgesetzInErstenJahren = 15)
  val outcome = Darlehen.simulate(totalT, pb15, kfw)
  printOutcome(outcome)
  println()
//   val restD2 = Darlehen(31000, 0.08, 1500, 5, 0.05, ausgesetzInErstenJahren = 15)
  val outcome2 = Darlehen.simulate(Tilgung.fix(0), pb20, kfw)
  printOutcome(outcome2)
}
