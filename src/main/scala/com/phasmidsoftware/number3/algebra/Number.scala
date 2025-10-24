package com.phasmidsoftware.number3.algebra

import com.phasmidsoftware.number3.core.NumberLike

/**
 * Represents a numeric entity that can be compared, approximated, and converted to other types.
 *
 * The `Number` trait extends functionality for ordered comparison and allows for distinguishing
 * between exact and approximate representations of numbers. Implementations of this trait must
 * provide definitions for essential numeric operations such as exact comparison and conversions
 * to approximate or concrete values.
 */
trait Number extends Ordered[Number] with NumberLike {
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
      compareExact(that)
    else if (!isExact) { // XXX this is not exact
      // NOTE this should be a FuzzyNumber in which case we don't need to approximate it.
      val maybeInt: Option[Int] = for {
        x <- approximation
        y <- that.approximation
      } yield x compare y
      maybeInt.get // NOTE this may throw an exception if the logic is wrong!
    }
    else // XXX this is exact and that is not exact
      -that.compare(this)
  }

  /**
   * Compares the current `Number` instance with another `Number` instance exactly.
   *
   * This method performs a comparison of two `Number` instances only if both numbers are exact.
   * It is expected to throw an exception or return undefined behavior if used inappropriately
   * with numbers that are not exact, depending on the implementation in the subtype.
   *
   * @param that the `Number` to compare against
   * @return an integer value:
   *         - a negative value if this `Number` is less than `that`
   *         - zero if this `Number` is equal to `that`
   *         - a positive value if this `Number` is greater than `that`
   */
  def compareExact(that: Number): Int

  /**
   * Attempts to convert the given number of type `T` to another value of the same type,
   * encapsulated in an `Option`.
   *
   * The method takes a number of type `T` as input, where `T` is a subtype of `Number`,
   * and performs a conversion operation. If the conversion is successful, it returns
   * `Some` containing the converted value. Otherwise, it returns `None`.
   *
   * @param t a prototype of the required output.
   * @return an `Option` containing the converted value of type `T` if the conversion is successful, or `None` otherwise
   */
  def convert[T <: Number](t: T): Option[T]

  /**
   * Provides an approximation of the current number, if applicable.
   *
   * This method attempts to compute an approximate representation of the number
   * in the form of a `FuzzyNumber`, which encapsulates uncertainty or imprecision
   * in its value. If no meaningful approximation is possible for the number, it
   * returns `None`.
   *
   * @return an `Option[FuzzyNumber]` containing the approximate representation
   *         of the number, or `None` if no approximation is available.
   */
  def approximation: Option[FuzzyNumber] = convert(FuzzyNumber.zero)

  /**
   * Performs addition of the current `Number` instance with another `Number` instance.
   *
   * This method calculates the sum of the current `Number` instance and the given `that` instance,
   * and returns a new `Number` representing the result of the addition.
   *
   * @param that the `Number` instance to add to the current instance
   * @return a new `Number` instance representing the result of the addition
   */
  def doPlus(that: Number): Number

  /**
   * Determines if the current number is equal to zero.
   *
   * @return true if the number is zero, false otherwise
   */
  def isZero: Boolean

  /**
   * Performs a multiplication operation on the current `Number` instance by repeated addition.
   *
   * This method calculates the result of multiplying the current `Number` instance by an integer `n`
   * by repeatedly adding the instance to itself `n - 1` times.
   *
   * @param n the multiplier, an integer value by which the current `Number` instance is to be multiplied
   * @return a new `Number` instance representing the result of the multiplication
   */
  def *(n: Int): Number =
    (1 until n).foldLeft[Number](this) { (a, _) => this doPlus a }

}

/**
 * A trait defining a potentially invertible algebraic structure.
 *
 * The `MaybeInvertible` trait represents a structure where an inverse element may or may not exist
 * for a given instance, depending on the specific implementation and its constraints. It provides
 * a method to retrieve the inverse when it exists.
 *
 * @tparam T the type of the elements in the structure that may have an inverse
 */
trait MaybeInvertible[T] {
  /**
   * Computes the inverse of the current instance if one exists.
   *
   * This method calculates the inverse of the current object within the context of the implementing
   * algebraic structure. The result is returned as an `Option`, where `None` indicates that the
   * inverse does not exist.
   *
   * @return an `Option` wrapping the inverse of type `T`, or `None` if the inverse does not exist.
   */
  def inverse: Option[T]
}