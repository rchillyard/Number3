package com.phasmidsoftware.number3.algebra

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AngleSpec extends AnyFlatSpec with Matchers {
  behavior of "Angle"

  it should "test creation" in {
    Angle(RationalNumber(1)) shouldBe Angle.pi
    Angle(RationalNumber(1)) shouldBe Angle.𝛑
  }

  it should "test render" in {
    Angle.zero.render shouldBe "0\uD835\uDED1"
    Angle.pi.render shouldBe "\uD835\uDED1"
  }

  it should "test conversion to other angles" in {
    // TODO implement test
  }

  it should "test comparison" in {
    // TODO implement test
  }

  it should "test compareExact" in {
    val pi = Angle.pi
    val x: Option[Number] = Angle.pi_2 * 2
    pi compareExact (x.get) shouldBe 0
  }
  it should "test arithmetic operations" in {
    //    Angle.pi plus Angle.pi shouldBe Angle.zero
    //    Angle.pi plus -Angle.pi shouldBe Angle.zero
    //    Angle.pi_2 plus Angle.pi_2 shouldBe Angle.pi
    Angle.pi_2 + -Angle.pi_2 shouldBe Angle.zero
  }
}
