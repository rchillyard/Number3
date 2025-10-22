package com.phasmidsoftware.number3.algebra

import scala.reflect.ClassTag

/**
 * Represents a numeric entity that can be compared, approximated, and converted to other types.
 *
 * The `Number` trait extends functionality for ordered comparison and allows for distinguishing
 * between exact and approximate representations of numbers. Implementations of this trait must
 * provide definitions for essential numeric operations such as exact comparison and conversions
 * to approximate or concrete values.
 */
trait Number extends Ordered[Number] {
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
   * Converts the current number to a representation of the specified type `T`, if possible.
   *
   * This method attempts to convert the number to a type `T` that has implicit evidence
   * of `Ordering`. If the conversion is successful, it returns an `Option` containing the
   * resulting typed value. If the conversion is not valid or not possible for the given
   * type `T`, it returns `None`.
   *
   * @return an `Option` containing the converted value of type `T` if successful, or `None` if the conversion is not possible.
   */
  def convert[T: ClassTag]: Option[T]

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
  def approximation: Option[FuzzyNumber] = convert[FuzzyNumber]

  /**
   * Determines if the number is represented exactly without any approximation.
   *
   * @return true if the number is exact, false otherwise
   */
  def isExact: Boolean

  /**
   * Determines if the current number is equal to zero.
   *
   * @return true if the number is zero, false otherwise
   */
  def isZero: Boolean
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