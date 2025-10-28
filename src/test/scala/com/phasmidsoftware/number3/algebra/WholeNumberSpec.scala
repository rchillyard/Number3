package com.phasmidsoftware.number3.algebra

import cats.kernel.CommutativeGroup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WholeNumberSpec extends AnyFlatSpec with Matchers {

  private val zero: WholeNumber = WholeNumber.zero
  private val one: WholeNumber = WholeNumber(1)

  behavior of "WholeNumber"

  it should "test creation" in {
    WholeNumber(0) shouldBe zero
  }

  it should "test render" in {
    zero.render shouldBe "0"
  }

  it should "test conversion to other Structures" in {
    zero.convert(Real.zero) shouldBe None
    zero.convert(RationalNumber.zero) shouldBe Some(RationalNumber.zero)
  }

  it should "test comparison" in {
    // TODO implement test
  }

  it should "test compareExact" in {
  }

  it should "test arithmetic operations" in {
    zero + one shouldBe one
    one + -one shouldBe zero
    one - one shouldBe zero
  }

  behavior of "CommutativeGroup[WholeNumber]"
  private val ac: CommutativeGroup[WholeNumber] = implicitly[CommutativeGroup[WholeNumber]]

  it should "combine" in {
    ac.combine(zero, one) shouldBe one
  }
  it should "inverse" in {
    ac.inverse(one) shouldBe WholeNumber(-1)
  }
}
