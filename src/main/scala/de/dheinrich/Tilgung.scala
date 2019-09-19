package de.dheinrich

trait Tilgung {
  def inMonat(monat: Int): BigDecimal
}

object Tilgung {

  /**
    * fix amount of money available each month
    * @param rate
    * @return
    */
  def fix(rate: BigDecimal) = new Tilgung {
    def inMonat(monat: Int): BigDecimal = rate
  }

  implicit class TilgungOps(t: Tilgung) {

    /**
      * increase the available money for each month each year by some percentage
      * @param rate
      * @return
      */
    def withInflationPA(rate: BigDecimal) = new Tilgung {
      def inMonat(monat: Int): BigDecimal = {
        val years   = monat / 12
        val without = t.inMonat(monat)
        (1 to years).foldLeft(without) { case (curr, _) => curr * (1 + rate) }
      }
    }

    /**
      * pay each month the sum of both base tilgungen
      * @param o
      * @return
      */
    def combine(o: Tilgung) = new Tilgung {
      def inMonat(monat: Int): BigDecimal = o.inMonat(monat) + t.inMonat(monat)
    }

    /**
      * reduce the amount to 60%
      * @param afterMonths starting month
      * @param forMonths duration of reduction
      * @return
      */
    def elternzeit(afterMonths: Int, forMonths: Int) =
      minderungNachFuer(0.6, afterMonths, forMonths)

    /**
      * loss of income for some duration
      * @param afterMonths starting month
      * @param forMonths duration of loss
      * @return
      */
    def ausfall(afterMonths: Int, forMonths: Int) = minderungNachFuer(0, afterMonths, forMonths)

    /**
      * reduction of income for some duration
      * @param percent amount of reduction
      * @param afterMonths starting month
      * @param forMonths duration of reduction
      * @return
      */
    def minderungNachFuer(percent: BigDecimal, afterMonths: Int, forMonths: Int) = new Tilgung {
      def inMonat(monat: Int): BigDecimal = {
        val without = t.inMonat(monat)
        if (afterMonths <= monat && monat < afterMonths + forMonths) without * percent else without
      }
    }

    /**
      * fixed reduction
      * @param afterMonths starting month
      * @param amount
      * @return
      */
    def extraKostenNach(afterMonths: Int, amount: BigDecimal) = new Tilgung {
      def inMonat(monat: Int): BigDecimal = {
        val without = t.inMonat(monat)
        if (afterMonths <= monat) without - amount else without
      }
    }

    /**
      * bonus pay every n months (i.e. chistmas)
      * @param amount
      * @param everyN
      * @return
      */
    def bonus(amount: BigDecimal, everyN: Int) = new Tilgung {
      def inMonat(monat: Int): BigDecimal = {
        val bonus = if (monat != 0 && monat % everyN == 0) amount else BigDecimal(0)
        t.inMonat(monat) + bonus
      }
    }
  }
}
