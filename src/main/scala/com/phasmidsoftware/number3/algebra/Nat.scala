package com.phasmidsoftware.number3.algebra

import algebra.ring.Semiring

/**
  * Represents natural numbers using Peano arithmetic.
  *
  * The `Nat` trait has two possible subtypes:
  * - `Zero`, representing the number 0.
  * - `Succ`, representing the successor of another `Nat` (e.g., `Succ(Zero)` represents 1, `Succ(Succ(Zero))` represents 2, and so on).
  *
  * This encoding provides a type-level representation of natural numbers
  * that can be used in functional programming constructs.
  *
  * Operations on natural numbers, such as addition and multiplication,
  * can be defined recursively via pattern matching.
  */
sealed trait Nat

/**
  * Represents the natural number 0 in Peano arithmetic.
  *
  * `Zero` is the base case of the `Nat` type, signifying the absence of any successors.
  * It serves as the foundation for constructing all other natural numbers through
  * the `Succ` type.
  */
case object Zero extends Nat

/**
  * Represents the successor of a natural number in Peano arithmetic.
  *
  * The `Succ` class is a case class that extends the `Nat` trait
  * and signifies the successor of another natural number.
  *
  * @param pred The predecessor of this natural number, which is also a `Nat`.
  */
case class Succ(pred: Nat) extends Nat

/**
  * Provides a companion object for the sealed trait `Nat`, incorporating utility methods
  * and type class instances for handling natural numbers defined via Peano arithmetic.
  *
  * The object includes an implicit implementation of the `Semiring` type class,
  * which introduces algebraic operations such as addition and multiplication
  * specifically tailored for natural numbers (`Nat`).
  */
object Nat {
  /**
    * Provides an implementation of the `Semiring` type class for natural numbers (`Nat`),
    * enabling operations such as addition and multiplication following the rules of Peano arithmetic.
    *
    * This instance defines the behavior for:
    * - `zero`: The base natural number `Zero`, representing the additive identity.
    * - `one`: The smallest positive natural number, represented as the successor of `Zero`.
    * - `plus`: Addition of two natural numbers, defined recursively.
    * - `times`: Multiplication of two natural numbers, defined recursively using addition.
    */
  implicit object natIsSemiring extends Semiring[Nat] {
    /**
      * Represents the zero value of a natural number.
      *
      * @return the Zero instance of the Nat type, representing the starting point of natural numbers.
      */
    def zero: Nat = Zero

    /**
      * Represents the natural number one in Peano arithmetic.
      *
      * @return A value of type `Nat` representing the successor of zero.
      */
    def one: Nat = Succ(Zero)

    /**
      * Adds two natural numbers represented using Peano arithmetic.
      *
      * The operation is defined recursively:
      * - Adding `Zero` to a number returns the number itself.
      * - Adding the successor of a number (`Succ`) involves recursively adding
      * the base number and wrapping the result in `Succ`.
      *
      * @param x the first natural number
      * @param y the second natural number
      * @return the sum of the two natural numbers
      */
    def plus(x: Nat, y: Nat): Nat = y match {
      case Zero => x
      case Succ(yPred) => Succ(plus(x, yPred))
    }

    /**
      * Multiplies two natural numbers using recursive multiplication as defined by Peano arithmetic.
      *
      * The multiplication operation is implemented recursively:
      * - If `y` is `Zero`, the result is `Zero`.
      * - If `y` is `Succ(yPred)`, the result is the sum of `x` and the recursive multiplication of `x` with `yPred`.
      *
      * @param x the first natural number to be multiplied
      * @param y the second natural number to be multiplied
      * @return the product of the two natural numbers `x` and `y`
      */
    def times(x: Nat, y: Nat): Nat = y match {
      case Zero => Zero
      case Succ(yPred) => plus(times(x, yPred), x)
    }

  }
}
