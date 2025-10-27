package com.phasmidsoftware.number3.algebra

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class NatSpec extends AnyFlatSpec with should.Matchers {

  import Nat.natIsSemiring

  "natIsSemiring" should "have correct zero element" in {
    natIsSemiring.zero shouldBe Nat(0)
  }

  it should "perform addition correctly" in {
    val n1 = Nat(5)
    val n2 = Nat(3)
    natIsSemiring.plus(n1, n2) shouldBe Nat(8)
  }

  it should "perform multiplication correctly" in {
    val n1 = Nat(4)
    val n2 = Nat(2)
    natIsSemiring.times(n1, n2) shouldBe Nat(8)
  }

  it should "satisfy distributive property" in {
    val a = Nat(2)
    val b = Nat(3)
    val c = Nat(4)
    val left = natIsSemiring.times(a, natIsSemiring.plus(b, c))
    val right = natIsSemiring.plus(natIsSemiring.times(a, b), natIsSemiring.times(a, c))
    left shouldBe right
  }
}
