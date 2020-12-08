package com.tst

object Problem1 extends App {

  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

  case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    val codeToGroup = rates.map(r => r.rateCode -> r.rateGroup).toMap
    prices.groupBy(cp => (codeToGroup.get(cp.rateCode), cp.cabinCode)).collect {
      case ((Some(rateGroup), cc), prices) =>
        val bestPrice = prices.minBy(_.price)
        BestGroupPrice(cc, bestPrice.rateCode, bestPrice.price, rateGroup)
    }.toSeq
  }
}

