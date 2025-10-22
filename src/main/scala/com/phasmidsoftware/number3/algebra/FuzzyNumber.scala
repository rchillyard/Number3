package com.phasmidsoftware.number3.algebra

import algebra.ring.Field
import com.phasmidsoftware.number.core.Fuzziness

import scala.reflect.ClassTag

/**
 * Represents a fuzzy number, which incorporates a primary value and an associated fuzziness level.
 *
 * A fuzzy number can be used in computations that require uncertainty handling or imprecision.
 * It extends the `Ordered` trait to enable comparisons and conforms to the `Number` interface.
 *
 * @constructor Creates a new `FuzzyNumber` with the specified value and fuzziness.
 * @param value the numeric value contained within this fuzzy number
 * @param fuzz  the associated fuzziness or uncertainty of the number
 */
case class FuzzyNumber(value: Double, fuzz: Fuzziness[Double]) extends Field[FuzzyNumber] with Number with MaybeInvertible[FuzzyNumber] {
  /**
   * Compares the current `FuzzyNumber` instance with another `FuzzyNumber`.
   *
   * This method performs a fuzzy comparison between two `FuzzyNumber` instances,
   * accounting for a tolerance parameter (currently fixed at 0.5 in implementation).
   * The comparison operates based on the internal representation of the numbers
   * and their respective degrees of fuzziness.
   *
   * @param that the `FuzzyNumber` to compare the current instance against
   * @return an integer value:
   *         - negative if the current `FuzzyNumber` is less than `that`
   *         - zero if the current `FuzzyNumber` is equal to `that`
   *         - positive if the current `FuzzyNumber` is greater than `that`
   */
  def compare(that: FuzzyNumber): Int = {
    // FIXME - this is a temporary hack
    com.phasmidsoftware.number.core.FuzzyNumber.fuzzyCompare(this.asInstanceOf[com.phasmidsoftware.number.core.FuzzyNumber], that.asInstanceOf[com.phasmidsoftware.number.core.FuzzyNumber], 0.5)
  }

  /**
   * Negates the value of the given `FuzzyNumber`.
   *
   * This method creates a new `FuzzyNumber` by negating the internal
   * value of the provided `FuzzyNumber`, effectively flipping its sign.
   *
   * @param x the `FuzzyNumber` to be negated
   * @return a new `FuzzyNumber` instance with the negated value
   */
  def negate(x: FuzzyNumber): FuzzyNumber = x.copy(value = -x.value)

  /**
   * Returns a new instance of a `FuzzyNumber` initialized with a value of 0
   * and a default fuzziness set to double-precision.
   *
   * @return a `FuzzyNumber` object with a value of 0 and double-precision fuzziness
   */
  def zero: FuzzyNumber = FuzzyNumber(0, Fuzziness.doublePrecision)

  /**
   * Returns a `FuzzyNumber` that represents the value one with double precision fuzziness.
   *
   * @return a `FuzzyNumber` instance with a value of one and double precision fuzziness.
   */
  def one: FuzzyNumber = FuzzyNumber(1, Fuzziness.doublePrecision)

  /**
   * Adds two FuzzyNumber instances by combining their values and fuzziness.
   *
   * This method computes the sum of the values of the provided FuzzyNumbers
   * and combines their fuzziness using a defined combination logic.
   * An exception will be thrown if the fuzziness combination logic results
   * in an invalid state.
   *
   * @param x the first FuzzyNumber to add
   * @param y the second FuzzyNumber to add
   * @return a new FuzzyNumber representing the combined value and fuzziness of x and y
   */
  def plus(x: FuzzyNumber, y: FuzzyNumber): FuzzyNumber = {
    val value = x.value + y.value
    val maybeFuzz: Option[Fuzziness[Double]] = Fuzziness.combine(x.value, y.value, relative = false, independent = true)(Some(x.fuzz) -> Some(y.fuzz))
    FuzzyNumber(value, maybeFuzz.get) // NOTE that if the logic is wrong, this will throw an exception!
  }

  /**
   * Divides one `FuzzyNumber` by another `FuzzyNumber`.
   *
   * This method performs a division operation between two `FuzzyNumber` instances,
   * producing a new `FuzzyNumber` that represents the result of the division.
   * The operation takes into account the fuzziness of the input numbers.
   *
   * @param x the numerator `FuzzyNumber` (dividend)
   * @param y the denominator `FuzzyNumber` (divisor)
   * @return a `FuzzyNumber` representing the result of dividing `x` by `y`
   */
  def div(x: FuzzyNumber, y: FuzzyNumber): FuzzyNumber =
    (y.inverse map (z => times(x, z))).getOrElse(FuzzyNumber(Double.PositiveInfinity, Fuzziness.createFuzz(0))) // TODO do this properly

  /**
   * Multiplies two `FuzzyNumber` instances and computes their fuzzy product.
   *
   * This method takes two `FuzzyNumber` inputs, multiplies their values,
   * and combines their fuzziness using the specified approach for fuzzy computations.
   *
   * @param x the first `FuzzyNumber` to multiply
   * @param y the second `FuzzyNumber` to multiply
   * @return a new `FuzzyNumber` representing the product of the input values with the combined fuzziness
   */
  def times(x: FuzzyNumber, y: FuzzyNumber): FuzzyNumber = {
    val value = x.value * y.value
    val maybeFuzz: Option[Fuzziness[Double]] = Fuzziness.combine(x.value, y.value, relative = true, independent = true)(Some(x.fuzz) -> Some(y.fuzz))
    FuzzyNumber(value, maybeFuzz.get) // NOTE that if the logic is wrong, this will throw an exception!
  }

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
  def convert[T: ClassTag]: Option[T] =
    // TODO - this is a temporary hack
    if (implicitly[ClassTag[T]].runtimeClass == classOf[FuzzyNumber])
      Some(this.asInstanceOf[T])
    else
      None

  /**
   * Determines if the number is represented exactly without any approximation.
   *
   * @return true if the number is exact, false otherwise
   */
  def isExact: Boolean = false

  /**
   * Determines if the current number is equal to zero.
   *
   * @return true if the number is zero, false otherwise
   */
  def isZero: Boolean = compare(zero) == 0

  /**
   * Computes the multiplicative inverse of the current `FuzzyNumber`, if possible.
   *
   * If the internal value of the `FuzzyNumber` is zero, an inverse does not exist,
   * and `None` is returned. Otherwise, this method calculates the inverse as a new
   * `FuzzyNumber` with a value equal to the reciprocal of the current value and the
   * same fuzziness, assuming the fuzziness is relative.
   *
   * @return an `Option` containing the inverse `FuzzyNumber`, or `None` if the inverse does not exist
   */
  def inverse: Option[FuzzyNumber] = value match {
    case 0 => None
    case _ => Some(FuzzyNumber(1 / value, fuzz)) // TODO this assumes that the fuzziness is relative.
  }

  /**
   * Compares the current `Number` instance with another `Number` instance exactly.
   *
   * This method should only be used when both `Number` instances are exact. An exact
   * comparison ensures a precise evaluation without considering any approximations
   * or fuzziness. If either `Number` is not exact, this method is expected to
   * throw an exception, as the operation is undefined for non-exact numbers.
   *
   * @param that the `Number` to compare against
   * @return an integer value:
   *         - negative if the current `Number` is less than `that`
   *         - zero if the current `Number` is equal to `that`
   *         - positive if the current `Number` is greater than `that`
   */
  def compareExact(that: Number): Int =
    throw new UnsupportedOperationException("FuzzyNumber.compareExact")
}

/**
 * A representation of a number with an associated degree of fuzziness.
 *
 * The `FuzzyNumber` class allows for computations and comparisons that account
 * for imprecision or uncertainty inherent to the numerical values.
 * It provides various operations such as arithmetic, comparison, and utility functions
 * to work with fuzzy numerical representations.
 */
object FuzzyNumber {
  /**
   * Constructs a `FuzzyNumber` with the given numeric value and a default fuzziness level.
   *
   * This method creates a `FuzzyNumber` instance using the provided value and a default
   * level of fuzziness based on double-precision.
   *
   * @param value the numeric value to be associated with the `FuzzyNumber`
   * @return a `FuzzyNumber` instance initialized with the specified value and default fuzziness
   */
  def apply(value: Double): FuzzyNumber = apply(value, Fuzziness.doublePrecision)

  /**
   * Creates a `FuzzyNumber` instance with a value of 0 and a default fuzziness level.
   *
   * @return a `FuzzyNumber` representing the value zero with default fuzziness
   */
  def zero: FuzzyNumber = apply(0)

  /**
   * Returns a `FuzzyNumber` representing the value one with double precision fuzziness.
   *
   * @return a `FuzzyNumber` instance initialized with the numeric value 1 and a default level of fuzziness.
   */
  def one: FuzzyNumber = apply(1)
}
