package com.phasmidsoftware.number3.algebra

import com.phasmidsoftware.number.core.inner.Factor
import com.phasmidsoftware.number3.core.Structure

/**
  * Represents a `Scalar`, which is one-dimensional `Structure` that can be ordered
  * and supports various mathematical operations and properties. Scalars include both
  * exact and approximate numerical entities.
  *
  * Multidimensional mathematical quantities such as Complex cannot be represented by a `Scalar` object.
  */
trait Scalar extends Structure {

  /**
    * Method to determine if this `Structure` object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a `PureNumber`, it would no longer be exact.
    *
    * @return true if this `Structure` object is exact in the context of no factor, else false.
    */
  def isExact: Boolean = approximation.isEmpty

  /**
    * Represents the scale of a scalar value as a `Double`.
    * This value indicates the magnitude by which a scalar is scaled,
    * and the conversion factor to yield a `PureNumber`.
    */
  val scale: Double

  /**
    * Attempts to yield a factor for the instance, if available.
    *
    * A `Factor` is a representation of the underlying numerical domain, for example, `PureNumber`, `Radian`, etc.
    *
    * @return an `Option[Factor]` containing the factor representation of this object,
    *         or `None` if factorization is not applicable or unavailable.
    */
  def maybeFactor: Option[Factor]

  /**
    * Provides an approximation of this number, if applicable.
    *
    * This method attempts to compute an approximate representation of the number
    * in the form of a `Real`, which encapsulates uncertainty or imprecision
    * in its value. If no meaningful approximation is possible for the number, it
    * returns `None`.
    *
    * @return an `Option[Real]` containing the approximate representation
    *         of this `Number`, or `None` if no approximation is available.
    */
  def approximation: Option[Real]

  /**
    * Adds this `Scalar` to another `Scalar` and returns the result as an `Option[Scalar]`.
    * The addition may not always be valid, depending on the context or properties of the `Scalar`s.
    *
    * @param that the `Scalar` to be added to the current instance
    * @return an `Option[Scalar]` containing the result of the addition, or `None` if the operation is not valid
    */
  def doPlus(that: Scalar): Option[Scalar]

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean
}


/**
  * The `Radians` trait represents a scalar quantity expressed in radians, a unit of angular measure.
  * It extends the `Scalar` trait, inheriting its properties and behaviors for numerical operations
  * and comparison, while specifically associating the scalar with a conversion factor defined by Pi.
  */
trait Radians extends Scalar {
  /**
    * Represents the scalar value for converting radians to a pure number, using Pi as the scaling factor.
    */
  val scale: Double = math.Pi // TODO change this to be an exact number (not a Double)
}
