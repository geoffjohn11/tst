package com.tst

import Problem1.{BestGroupPrice, CabinPrice, Rate}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class Problem1Spec extends AnyWordSpec {
  val rates = List(
    Rate("M1","Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )

  val cabinPrices = List(
    CabinPrice("CA", "M1", BigDecimal(200.00)),
    CabinPrice("CA", "M2", BigDecimal(250.00)),
    CabinPrice("CA", "S1", BigDecimal(225.00)),
    CabinPrice("CA", "S2", BigDecimal(260.00)),
    CabinPrice("CB", "M1", BigDecimal(230.00)),
    CabinPrice("CB", "M2", BigDecimal(260.00)),
    CabinPrice("CB", "S1", BigDecimal(245.00)),
    CabinPrice("CB", "S2", BigDecimal(270.00)))

  "getBestGroupPrices" should {
    "return the best cabin price for each rate group" in {
      Problem1.getBestGroupPrices(rates,cabinPrices) shouldBe List(BestGroupPrice("CB","S1",245.0,"Senior"), BestGroupPrice("CA","M1",200.0,"Military"), BestGroupPrice("CA","S1",225.0,"Senior"), BestGroupPrice("CB","M1",230.0,"Military"))
    }
    "handle an empty List" in {
      Problem1.getBestGroupPrices(Nil, Nil) shouldBe Nil
      Problem1.getBestGroupPrices(rates, Nil) shouldBe Nil
      Problem1.getBestGroupPrices(Nil, cabinPrices) shouldBe Nil
    }
  }
}


