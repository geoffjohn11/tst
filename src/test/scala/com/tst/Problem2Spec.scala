package com.tst

import com.tst.Problem2.{Promotion, PromotionCombo}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class Problem2Spec extends AnyWordSpec {
  val p1 = Promotion("1", List("3"))
  val p2 = Promotion("2", List("4", "5"))
  val p3 = Promotion("3", List("1"))
  val p4 = Promotion("4", List("2"))
  val p5 = Promotion("5", List("2"))
  val codes = List(p1, p2, p3, p4, p5)

  "combinablePromotions" should{
    "find PromotionCombo with maximum number of promotions" in {
      Problem2.allCombinablePromotions(codes).map(_.promotionCodes.map(_.toInt).sorted).sortBy(_.length) shouldBe List(List(1,2), List(2,3) ,List(1,4,5), List(3,4,5))
      Problem2.allCombinablePromotions(List(p1, p3)).map(_.promotionCodes.map(_.toInt).sorted).sortBy(_.length) shouldBe List(List(3), List(1))
      Problem2.allCombinablePromotions(Nil) shouldBe List(PromotionCombo(Nil))
      Problem2.combinablePromotions("1", codes).map(_.promotionCodes.map(_.toInt)).sortBy(_.length) shouldBe List(List(1,2) , List(1,4,5))
    }
  }
  "allowedPromotionF" should {
    "allow/disallow promotion when combinable/not combinable" in {
      Problem2.allowedPromotionF(Map("1" -> Set("3")))(p1, Set(p2)) shouldBe true
      Problem2.allowedPromotionF(Map("1" -> Set("3", "2")))(p1, Set(p2)) shouldBe false
    }
  }
  "genAllowedPromos" should {
    "generate promo sets that satisfy the allow function" in {
      val allowAllowed = (p: Promotion, lp: Set[Promotion]) => true
      val oneDisAllowed = (p: Promotion, lp: Set[Promotion]) => p.code != "1"
      Problem2.genAllowedPromos(Set(p1, p2, p3), allowAllowed).map(_.map(_.code)) shouldBe Set(Set("1", "2", "3"))
      Problem2.genAllowedPromos(Set(p1, p2, p3), oneDisAllowed).map(_.map(_.code)) shouldBe Set(Set("2", "3"))
    }
  }

}
