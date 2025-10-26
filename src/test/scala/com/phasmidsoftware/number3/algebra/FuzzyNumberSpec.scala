package com.phasmidsoftware.number3.algebra

import algebra.ring.Ring
import com.phasmidsoftware.number.core.{AbsoluteFuzz, Gaussian}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FuzzyNumberSpec extends AnyFlatSpec with Matchers {
  behavior of "FuzzyNumber"

  // Basic arithmetic operations
  ignore should "perform addition correctly" in {
    val x = FuzzyNumber(1)
    val y = FuzzyNumber(2)
    FuzzyNumber.zero.plus(x, y) compareTo RationalNumber(3) shouldBe 0
  }

  it should "perform subtraction correctly" in {
    val x = FuzzyNumber(5)
    val y = FuzzyNumber(3)
    FuzzyNumber.zero.plus(x, FuzzyNumber.zero.negate(y)) should matchPattern { case FuzzyNumber(2, _) => }
  }

  it should "perform multiplication correctly" in {
    val x = FuzzyNumber(2)
    val y = FuzzyNumber(3)
    FuzzyNumber.zero.times(x, y) should matchPattern { case FuzzyNumber(6, _) => }
  }

  ignore should "perform division correctly" in {
    val x = FuzzyNumber(6)
    val y = FuzzyNumber(2)
    FuzzyNumber.zero.div(x, y) should matchPattern { case FuzzyNumber(3, _) => }
  }

  // Comparison operations
  ignore should "compare numbers correctly" in {
    val x = FuzzyNumber(1)
    val y = FuzzyNumber(2)
    x.compare(y) shouldBe -1
    y.compare(x) shouldBe 1
    x.compare(x) shouldBe 0
  }

  // Conversion operations
  it should "convert to different number types" in {
    val x = FuzzyNumber(5)
    x.convert(FuzzyNumber.zero) shouldBe Some(x)
    x.convert(Angle.zero) shouldBe None
  }

  // Edge cases and special values
  ignore should "handle zero correctly" in {
    val x = FuzzyNumber(0)
    x.isZero shouldBe true
  }

  behavior of "Ring[FuzzyNumber]"
  private val rf: Ring[FuzzyNumber] = implicitly[Ring[FuzzyNumber]]

  it should "plus" in {
    rf.plus(FuzzyNumber(3), Angle.pi.approximation.get) shouldBe FuzzyNumber(6.141592653589793, AbsoluteFuzz(4.0127375222238153E-16, Gaussian))
  }
  it should "negate" in {
    rf.negate(FuzzyNumber(3)) shouldBe FuzzyNumber(-3)
  }

}
