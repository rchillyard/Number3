package com.phasmidsoftware.number3.algebra

import algebra.ring.Ring
import com.phasmidsoftware.number.core.{AbsoluteFuzz, Box}
import com.phasmidsoftware.number3.algebra.Real.fuzzyNumberIsRing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RealSpec extends AnyFlatSpec with Matchers {
  behavior of "Real"

  // Basic arithmetic operations
  ignore should "perform addition correctly" in {
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

  ignore should "perform division correctly" in {
    val x = Real(6)
    val y = Real(2)
    fuzzyNumberIsRing.div(x, y) should matchPattern { case Real(3, _) => }
  }

  // Comparison operations
  ignore should "compare numbers correctly" in {
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
  ignore should "handle zero correctly" in {
    val x = Real(0)
    x.isZero shouldBe true
  }

  behavior of "Ring[Real]"
  private val rf: Ring[Real] = implicitly[Ring[Real]]

  it should "plus" in {
    rf.plus(Real(3), Angle.pi.approximation.get) shouldBe Real(6.141592653589793, Some(AbsoluteFuzz(5.02654824574367E-16, Box)))
  }
  it should "negate" in {
    rf.negate(Real(3)) shouldBe Real(-3)
  }

}
