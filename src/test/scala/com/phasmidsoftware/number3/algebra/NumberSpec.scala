package com.phasmidsoftware.number3.algebra

import com.phasmidsoftware.number.core.Fuzziness
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NumberSpec extends AnyFlatSpec with Matchers {

  behavior of "RationalNumber"

  // Basic arithmetic operations
  it should "perform addition correctly" in {
    val x = RationalNumber(1)
    val y = RationalNumber(2)
    RationalNumber.zero.plus(x, y) shouldBe RationalNumber(3)
  }

  it should "perform subtraction correctly" in {
    val x = RationalNumber(5)
    val y = RationalNumber(3)
    RationalNumber.zero.plus(x, RationalNumber.zero.negate(y)) shouldBe RationalNumber(2)
  }

  it should "perform multiplication correctly" in {
    val x = RationalNumber(2)
    val y = RationalNumber(3)
    RationalNumber.zero.times(x, y) shouldBe RationalNumber(6)
  }

  it should "perform division correctly" in {
    val x = RationalNumber(6)
    val y = RationalNumber(2)
    RationalNumber.zero.div(x, y) shouldBe RationalNumber(3)
  }

  // Comparison operations
  it should "compare numbers correctly" in {
    val x = RationalNumber(1)
    val y = RationalNumber(2)
    x.compare(y) shouldBe -1
    y.compare(x) shouldBe 1
    x.compare(x) shouldBe 0
  }

  // Conversion operations
  it should "convert to different number types" in {
    val x = RationalNumber(5)
    x.convert[FuzzyNumber] shouldBe Some(FuzzyNumber(5, Fuzziness.doublePrecision))
    x.convert[Angle] shouldBe None
    x.convert[Int] shouldBe None
    x.convert[Double] shouldBe None
  }

  // Edge cases and special values
  it should "handle zero correctly" in {
    val x = RationalNumber(0)
    x.isZero shouldBe true
  }

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
    x.convert[FuzzyNumber] shouldBe Some(FuzzyNumber(5, Fuzziness.doublePrecision))
    x.convert[Angle] shouldBe None
    x.convert[Int] shouldBe None
    x.convert[Double] shouldBe None
  }

  // Edge cases and special values
  ignore should "handle zero correctly" in {
    val x = FuzzyNumber(0)
    x.isZero shouldBe true
  }
}
