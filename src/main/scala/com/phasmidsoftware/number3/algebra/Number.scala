package com.phasmidsoftware.number3.algebra

import com.phasmidsoftware.number.core.NumberException
import com.phasmidsoftware.number3.misc.FP

/**
  * Represents a pure number that can be compared, approximated, and converted to other types.
  *
  * `Number` is a trait that extends the `Scalar` trait, adding functionality for ordered comparison.
  */
trait Number extends Scalar with Ordered[Number] {

  /**
    * Compares the current `Number` instance with another `Number` instance.
    *
    * This method performs a comparison of two `Number` instances. If both numbers are exact, it uses exact comparison.
    * If one or both numbers are not exact, it attempts to approximate and compare. Note that in some cases, this method
    * may throw an exception if an invalid approximation logic is encountered.
    *
    * @param that the `Number` instance to compare the current instance against
    * @return an integer value:
    *         - a negative value if this `Number` is less than `that`
    *         - zero if this `Number` is equal to `that`
    *         - a positive value if this `Number` is greater than `that`
    */
  def compare(that: Number): Int = {
    if (isExact && that.isExact) // XXX both are exact
      FP.recover(compareExact(that))(NumberException(s"Angle.compareExact: logic error: $this, $that"))
    else if (!isExact) { // XXX this is not exact
      // NOTE this should be a Real in which case we don't need to approximate it.
      val maybeInt: Option[Int] = for {
        x <- approximation
        y <- that.approximation
      } yield x compare y
      FP.recover(maybeInt)(NumberException("Number.compare: Logic error"))
    }
    else // XXX this is exact and that is not exact
      -that.compare(this)
  }

  /**
    * Compares this `Scalar` with another `Scalar` for exact equivalence.
    * This method checks if both instances can be compared in an exact manner.
    *
    * @param that the `Scalar` instance to compare against
    * @return an `Option[Int]` value:
    *         - `Some(-1)` if this `Scalar` is less than `that`
    *         - `Some(0)` if this `Scalar` is equal to `that`
    *         - `Some(1)` if this `Scalar` is greater than `that`
    *         - `None` if the exact comparison is not possible
    */
  def compareExact(that: Scalar): Option[Int]

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
  def approximation: Option[Real] =
    convert(Real.zero)

  /**
    * Performs a multiplication operation on the current `Number` instance by repeated addition.
    *
    * This method calculates the result of multiplying the current `Number` instance by an integer `n`
    * by repeatedly adding the instance to itself `n - 1` times.
    *
    * @param n the multiplier, an integer value by which the current `Number` instance is to be multiplied
    * @return a new `Number` instance representing the result of the multiplication
    */
  def *(n: Int): Option[Number] =
    (1 until n).foldLeft[Option[Number]](Some(this)) {
      case (Some(a), _) =>
        (this doPlus a).asInstanceOf[Option[Number]] // TODO check that this is OK
      case (None, _) => None
    }

  /**
    * A scale factor applied to the `Number` instance.
    *
    * The `scale` represents a multiplier that influences computations or adjustments involving this number.
    * It is commonly used to scale or manipulate the magnitude of the number in various arithmetic or operational contexts.
    */
  val scale: Double = 1.0
}
