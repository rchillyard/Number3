package com.phasmidsoftware.number3.algebra

import algebra.ring.Field
import cats.Show
import com.phasmidsoftware.number.core.inner.{PureNumber, Value}
import com.phasmidsoftware.number.core.{Fuzziness, NumberException}

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
    maybeFuzz match {
      case Some(fuzz) =>
        FuzzyNumber(value, fuzz)
      case None =>
        throw NumberException(s"FuzzyNumber.plus: invalid fuzziness: ${x.fuzz} + ${y.fuzz} = $maybeFuzz")
    }
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
    maybeFuzz match {
      case Some(fuzz) =>
        FuzzyNumber(value, fuzz)
      case None =>
        throw NumberException(s"FuzzyNumber.times: invalid fuzziness: ${x.fuzz} + ${y.fuzz} = $maybeFuzz")
    }
  }

  /**
   * Converts the given numeric value to an optional representation.
   *
   * This method accepts a number of type T, where T is a subtype of Number,
   * and returns an Option containing the input number if certain conditions
   * (not detailed in this method's implementation) are met, otherwise None.
   *
   * @param t a prototype of the required output.
   * @return an Option wrapping the input number if the conversion is successful, otherwise None
   */
  def convert[T <: Number](t: T): Option[T] = None

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
  def compareExact(that: Number): Option[Int] =
    throw new UnsupportedOperationException("FuzzyNumber.compareExact")

  /**
   * Method to render this NumberLike in a presentable manner.
   *
   * @return a String
   */
  def render: String = new com.phasmidsoftware.number.core.FuzzyNumber(Value.fromDouble(Some(value)), PureNumber, Some(fuzz)).render

  /**
   * Adds the current `Number` instance to another `Number`.
   *
   * This method performs addition between the current `Number` and the provided `that` `Number`.
   * The implementation takes into account the internal properties of the two `Number` instances
   * and combines them accordingly.
   *
   * @param that the `Number` to be added to the current instance
   * @return a new `Number` representing the result of adding the current instance and `that`
   */
  def doPlus(that: Number): Option[Number] = that match {
    case f@FuzzyNumber(_, _) =>
      Some(this.plus(this, f))
    case n =>
      n.convert(this) map (x => this.plus(this, x))
  }

  /**
   * Scales the current `FuzzyNumber` instance by the mathematical constant π (pi).
   *
   * This method creates a new `FuzzyNumber` by multiplying the internal value of
   * the current instance with the constant π.
   * The fuzziness remains unaffected in the resulting instance.
   *
   * @return a new `FuzzyNumber` instance with its value scaled by π
   */
  private[algebra] def scaleByPi: FuzzyNumber = copy(value = value * Math.PI)
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

  /**
   * Returns a `FuzzyNumber` representing the mathematical constant π (pi)
   * with a default level of fuzziness.
   *
   * @return a `FuzzyNumber` initialized with the value of π and the default fuzziness level
   */
  def pi: FuzzyNumber = apply(Math.PI)

  /**
   * Provides an implicit instance of `Show` for the `FuzzyNumber` type.
   *
   * This implementation defines how instances of `FuzzyNumber` are converted
   * to a human-readable string representation by invoking their `render` method.
   *
   * It enables seamless integration with type classes requiring a `Show` instance,
   * allowing `FuzzyNumber` objects to be printed or logged in a human-readable format.
   */
  implicit val showFuzzyNumber: Show[FuzzyNumber] = Show.show(_.render)

}
