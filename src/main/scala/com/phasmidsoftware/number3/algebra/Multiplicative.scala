package com.phasmidsoftware.number3.algebra

import com.phasmidsoftware.number3.core.Structure

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
