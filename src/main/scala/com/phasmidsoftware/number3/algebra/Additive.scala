package com.phasmidsoftware.number3.algebra

/**
  * Represents an additive structure for a type `T` that is a subtype of `Number`.
  *
  * This trait defines basic operations for additive algebraic structures,
  * such as addition and finding the additive inverse. It requires concrete
  * implementations for the methods to provide functionality specific to the type `T`.
  *
  * @tparam T the type over which the additive operations are defined,
  *           constrained to subtypes of `Number`
  */
trait Additive[T <: Number] {
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
