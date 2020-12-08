package com.tst

object Problem2 extends App {

  case class Promotion(code: String, notCombinableWith: Seq[String])

  case class PromotionCombo(promotionCodes: Seq[String])

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val codeToNotCombinable = allPromotions.foldLeft(Map[String, Set[String]]()) { case (acc, Promotion(code, ncw)) => acc + (code -> ncw.toSet) }
    val allowedPermutations = genAllowedPromos(allPromotions.toSet, allowedPromotionF(codeToNotCombinable)).map(_.map(_.code))
    allowedPermutations.map(i => PromotionCombo(i.toSeq)).toSeq
  }

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]) =
    allCombinablePromotions(allPromotions.filterNot(_.notCombinableWith.contains(promotionCode)))

  /**
   * Determines if a promotion can be added to a set of promotions.
   * If the code does not have an entry in the lookup map it is allowed.
   *
   * @param codeToNotCombinable
   * @param p
   * @param ps
   * @return
   */
  def allowedPromotionF(codeToNotCombinable: Map[String, Set[String]])(p: Promotion, ps: Set[Promotion]) =
    codeToNotCombinable.get(p.code).fold(true)(ps.map(_.code).intersect(_).isEmpty)

  /**
   * Generate all promotion combinations that are allowable
   *
   * @param lst
   * @param allowedPromotion
   * @return
   */
  def genAllowedPromos(lst: Set[Promotion], allowedPromotion: (Promotion, Set[Promotion]) => Boolean): Set[Set[Promotion]] = {
    if(lst.isEmpty) Set(Set[Promotion]()) else{
      for {
        i: Int <- (0 until lst.size).toSet
        (before, rest) = lst.splitAt(i)
        elem = rest.head
        subPermutates = genAllowedPromos(before ++ rest.tail, allowedPromotion)
        res <- subPermutates.map {
          case subPermutate if allowedPromotion(elem, subPermutate) => Set(elem) ++ subPermutate
          case subPermutate => subPermutate
        }
      } yield res
    }
  }
}

