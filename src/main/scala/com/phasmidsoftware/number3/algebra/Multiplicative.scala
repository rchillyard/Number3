package com.phasmidsoftware.number3.algebra

/**
  * A trait that represents multiplicative operations for a type `T` that extends `Number`.
  *
  * The `Multiplicative` trait provides functionality for performing multiplication and division
  * in a type-safe manner for instances of `T`.
  *
  * @tparam T the type of numeric elements that must extend the `Number` trait
  */
trait Multiplicative[T <: Number] {
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
