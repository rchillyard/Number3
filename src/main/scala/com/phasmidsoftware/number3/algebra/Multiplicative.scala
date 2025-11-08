package com.phasmidsoftware.number3.algebra

import com.phasmidsoftware.number3.algebra.Structure

/**
  * Defines the behavior for a multiplicative algebraic structure.
  *
  * This trait represents entities that support multiplication and division operations within
  * the context of a type `T` which extends the `Structure` trait.
  * Implementations of this trait must provide specific rules and logic for combining and
  * operating on instances of `T`.
  *
  * @tparam T the type of the structure that supports multiplication and division, extending `Structure`
  */
trait Multiplicative[T <: Structure] {
  /**
    * Represents the multiplicative identity element of the structure.
    *
    * The `one` value serves as the neutral element for the multiplication operation, meaning
    * that for any instance `t` of type `T`, the equation `one * t = t * one = t` holds true.
    */
  def one: T

  /**
    * Multiplies the specified `T` by this `T` instance.
    *
    * @param t an instance of `T` to be multiplied by this `T`
    * @return a new `Multiplicative[T]` representing the product of this `T` and the given `T`
    */
  def *(t: T): Multiplicative[T]

  /**
    * Divides this `T` instance by the specified `T`.
    *
    * @param t an instance of `T` to be the divisor
    * @return a new `Multiplicative[T]` representing the quotient of this `T` and `t`
    */
  def /(t: T): Multiplicative[T]
}

/**
  * Defines the behavior for a multiplicative algebraic structure.
  *
  * This trait represents entities that support multiplication and division operations within
  * the context of a type `T` which extends the `Structure` trait.
  * Implementations of this trait must provide specific rules and logic for combining and
  * operating on instances of `T`.
  *
  * @tparam T the type of the structure that supports multiplication and division, extending `Structure`
  */
trait MultiplicativeWithInverse[T <: Structure] extends Multiplicative[T] {

  /**
    * Computes the multiplicative inverse of this instance.
    *
    * The inverse is defined as an element that, when multiplied with this instance, yields the
    * multiplicative identity element (`one`) of the structure.
    *
    * @return a new `Multiplicative[T]` representing the multiplicative inverse of this instance
    */
  def inverse: Multiplicative[T]
}

/**
  * Represents an extension of the `Multiplicative` algebraic structure that includes
  * the ability to raise elements to a given power.
  *
  * This trait enables the definition of exponentiation operations for any type `T`
  * that extends `Structure`. The power operation allows a value to be multiplied
  * by itself a specified number of times, represented as an exponent.
  *
  * @tparam T the type parameter that extends `Structure`, representing the algebraic structure supported by this trait
  */
trait MultiplicativeWithPower[T <: Structure] extends Multiplicative[T] {
  /**
    * Calculates the result of raising this instance to the power of the specified integer `n`.
    *
    * @param n the exponent to which the instance is raised; must be a non-negative integer
    * @return a new `MultiplicativeWithPower[T]` instance representing this instance raised to the `n`th power
    */
  def power(n: Int): MultiplicativeWithPower[T] =
    (1 until n).foldLeft(this) {
      (r, s) =>
        // TODO sort out these class casts.
        (r * this.asInstanceOf[T]).asInstanceOf[MultiplicativeWithPower[T]]
    }
}