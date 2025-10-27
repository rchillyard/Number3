package com.phasmidsoftware.number3.algebra

import cats.kernel.CommutativeGroup
import com.phasmidsoftware.number.core.Fuzziness
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AngleSpec extends AnyFlatSpec with Matchers {

  private val zero: Angle = Angle.zero
  private val pi: Angle = Angle.pi
  private val piBy2: Angle = Angle.pi_2

  behavior of "Angle"

  it should "test creation" in {
    Angle(RationalNumber(1)) shouldBe pi
    Angle(RationalNumber(1)) shouldBe Angle.𝛑
  }

  it should "test render" in {
    zero.render shouldBe "0\uD835\uDED1"
    pi.render shouldBe "\uD835\uDED1"
  }

  it should "test conversion to other Structures" in {
    pi.convert(FuzzyNumber.zero) shouldBe Some(FuzzyNumber(3.141592653589793, Fuzziness.doublePrecision))
    pi.convert(RationalNumber.zero) shouldBe None
  }

  it should "test comparison" in {
    // TODO implement test
  }

  it should "test compareExact" in {
    val xo: Option[Number] = piBy2 * 2
    xo flatMap (x => pi compareExact x) shouldBe Some(0)
  }
  it should "test arithmetic operations" in {
    pi + pi shouldBe zero
    pi + -pi shouldBe zero
    piBy2 + piBy2 shouldBe pi
    piBy2 + -piBy2 shouldBe zero
    piBy2 - piBy2 shouldBe zero
  }

  behavior of "CommutativeGroup[Angle]"
  private val ac: CommutativeGroup[Angle] = implicitly[CommutativeGroup[Angle]]

  it should "combine" in {
    ac.combine(-pi, pi) shouldBe zero
  }
  it should "inverse" in {
    ac.inverse(pi) shouldBe -pi
  }
}
