package com.phasmidsoftware.number3.algebra

import com.phasmidsoftware.number3.algebra.Real.fuzzyNumberIsRing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NumberSpec extends AnyFlatSpec with Matchers {

  behavior of "Number"

  // Basic arithmetic operations
  it should "perform addition correctly" in {
    val x = Real(1)
    val y = Real(2)
    fuzzyNumberIsRing.plus(x, y) compareTo RationalNumber(3) shouldBe 0
  }

  it should "perform subtraction correctly" in {
    val x = Real(5)
    val y = Real(3)
    fuzzyNumberIsRing.plus(x, fuzzyNumberIsRing.negate(y)) should matchPattern { case Real(2, _) => }
  }

  it should "perform multiplication correctly" in {
    val x = Real(2)
    val y = Real(3)
    fuzzyNumberIsRing.times(x, y) should matchPattern { case Real(6, _) => }
  }

  it should "perform division correctly" in {
    val x = Real(6)
    val y = Real(2)
    fuzzyNumberIsRing.div(x, y) should matchPattern { case Real(3, _) => }
  }

  // Comparison operations
  it should "compare numbers correctly" in {
    val x = Real(1)
    val y = Real(2)
    x.compare(y) shouldBe -1
    y.compare(x) shouldBe 1
    x.compare(x) shouldBe 0
  }

  // Conversion operations
  it should "convert to different number types" in {
    val x = Real(5)
    x.convert(Real.zero) shouldBe Some(x)
    x.convert(Angle.zero) shouldBe None
  }

  // Edge cases and special values
  it should "handle zero correctly" in {
    val x = Real(0)
    x.isZero shouldBe true
  }
}
