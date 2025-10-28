package com.phasmidsoftware.number3.algebra

import com.phasmidsoftware.number3.core.Structure

/**
  * Represents an additive structure for types that extend `Structure`.
  *
  * This trait defines operations that are part of an additive algebraic structure,
  * including addition, subtraction, and negation.
  *
  * @tparam T the subtype of `Structure` that supports additive operations
  */
trait Additive[T <: Structure] {
  /**
    * Adds the specified `T` to this `T` instance.
    *
    * @param t an instance of `T` to be added to this `T`
    * @return a new `T` representing the sum of this `T` and the given `T`
    */
  def +(t: T): Additive[T]

  /**
    * Computes the additive inverse of this instance.
    *
    * This method returns a new instance representing the negation of this value,
    * as defined in the additive structure of the type `T`.
    *
    * @return a new instance of type `T` that is the additive inverse of this instance
    */
  def unary_- : Additive[T]

  /**
    * Subtracts the specified `T` from this `T` instance.
    *
    * @param t an instance of `T` to be subtracted from this `T`
    * @return a new `Additive[T]` representing the result of the subtraction of the given `T` from this `T`
    */
  def -(t: T): Additive[T]
}
