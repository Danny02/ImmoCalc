package de.dheinrich
import scala.annotation.tailrec

case class Darlehen(
    summe: BigDecimal,
    zins: BigDecimal,
    rate: BigDecimal,
    laufzeit: Int,
    sonderTilgungPerYearPercent: BigDecimal,
    tilgungsfreieMonate: Int = 0,
    ausgesetzInErstenJahren: Int = 0
) {
  def mzins = zins / 12

  def sonderTilgungPerYear = sonderTilgungPerYearPercent * summe

  def initState = Darlehen.State(this, summe)
}

object Darlehen {

  case class State(
      darlehen: Darlehen,
      schuld: BigDecimal,
      zinsenPayed: BigDecimal = BigDecimal(0),
      year: Int = 0,
      month: Int = 0,
      sonderTilgungen: List[BigDecimal] = Nil
  ) {

    def isAktive = year >= darlehen.ausgesetzInErstenJahren && !isFinished

    def isFinished =
      schuld == 0 || (year + 1 == darlehen.laufzeit + darlehen.ausgesetzInErstenJahren && month == 12)

    def payedOffAt() = {
      val s = payedOffState()
      (s.year - darlehen.ausgesetzInErstenJahren, s.month)
    }

    def payedOffState() = {
      @tailrec
      def inner(state: State): State =
        if (state.schuld == 0)
          state
        else
          inner(Darlehen.nextState(state, 0, true)._1)

      inner(this)
    }

    def nextDate = {
      if (month == 12)
        (year + 1, 1)
      else
        (year, month + 1)
    }

    def incrDate() = {
      val (ny, nm) = nextDate
      copy(year = ny, month = nm)
    }

    def monthCount = year * 12 + month

    def isTilgunsfrei = monthCount <= darlehen.tilgungsfreieMonate

    def isAusgesetzt = year < darlehen.ausgesetzInErstenJahren

    def nextZins = schuld * darlehen.mzins

    def nextPayment: BigDecimal = {
      val ns = incrDate()
      if (ns.isAktive) {
        if (ns.isTilgunsfrei) nextZins else darlehen.rate min (schuld + nextZins)
      } else {
        0
      }
    }
  }

  def simulate(tilgung: Tilgung, darlehen: Darlehen*) = {

    @tailrec
    def inner(states: List[State], month: Int = 0): List[State] = {
      if (states.exists(_.isFinished == false)) {
        val mustRate = states.map(_.nextPayment).sum
        val maxRate = tilgung.inMonat(month)
        val extra    = (maxRate - mustRate) max 0
        val ns = states
          .foldLeft((List[State](), extra)) {
            case ((nstates, restExtra), s) => {
              if (s.isFinished) {
                (nstates :+ s, restExtra)
              } else {
                val (next, rest) = Darlehen.nextState(s, restExtra)
                (nstates :+ next, rest)
              }
            }
          }
          ._1
        inner(ns, month + 1)
      } else {
        states
      }
    }

    inner(darlehen.toList.map(_.initState))
  }

  def nextState(
      state: State,
      sonderTilgung: BigDecimal,
      ignoreFinished: Boolean = false
  ): (State, BigDecimal) = {
    assert(ignoreFinished || !state.isFinished)

    val darlehen  = state.darlehen
    val monthZins = state.nextZins

    val nState = state.incrDate()
    val st =
      if (nState.month == 1) BigDecimal(0) +: state.sonderTilgungen else state.sonderTilgungen

    if (nState.isAusgesetzt) {
      return (nState.copy(sonderTilgungen = st), 0)
    }

    val tilgung = if (!nState.isTilgunsfrei) darlehen.rate - monthZins else BigDecimal(0)

    val sonderCurrent = st.head
    val (sonder, nextCurrent) =
      if (sonderCurrent < darlehen.sonderTilgungPerYear) {
        val n = (sonderCurrent + sonderTilgung) min darlehen.sonderTilgungPerYear
        (n - sonderCurrent, n)
      } else {
        (BigDecimal(0), sonderCurrent)
      }

    val nextSchuld = (state.schuld - tilgung - sonder) max 0.0
    (
      nState.copy(
        schuld = nextSchuld,
        zinsenPayed = state.zinsenPayed + monthZins,
        sonderTilgungen = nextCurrent +: st.tail
      ),
      sonderTilgung - sonder
    )
  }
}
