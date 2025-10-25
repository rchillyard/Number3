package com.phasmidsoftware.number3.algebra

import algebra.ring.Field
import cats.Show
import com.phasmidsoftware.number.core.Fuzziness
import com.phasmidsoftware.number.core.inner.Rational

/**
  * Represents a rational number and provides arithmetic operations
  * along with functions to retrieve its identity and inverse elements.
  *
  * This class extends `Number` and `Field` to provide additional
  * operations specific to rational numbers.
  *
  * @constructor Creates a new RationalNumber instance with the given
  *              rational value `r`.
  * @param r the underlying rational value
  */
case class RationalNumber(r: Rational) extends Field[RationalNumber] with Additive[RationalNumber] with Number with MaybeInvertible[RationalNumber] {
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
  def compareExact(that: Number): Option[Int] = that match {
    case RationalNumber(o) =>
      Some(r.compareTo(o))
    case _ =>
      None
  }

  /**
    * Converts the given number to an instance of the specified type, if possible.
    *
    * The method attempts to convert the input into a `FuzzyNumber` if it matches the specific type constraint,
    * or returns `None` otherwise.
    *
    * @param t a prototype of the required output.
    * @tparam T the type of the number, which must be a subtype of `Number`
    * @return an `Option` containing the converted value of type `T` if successful, or `None` if the conversion is not possible
    */
  def convert[T <: Number](t: T): Option[T] = t match {
    case _: FuzzyNumber =>
      Some(FuzzyNumber(r.toDouble, Fuzziness.doublePrecision).asInstanceOf[T])
    case _ =>
      None
  }


  /**
    * Computes the additive inverse of the given rational number.
    *
    * @param x the `RationalNumber` to be negated
    * @return a new `RationalNumber` representing the additive inverse of the input
    */
  def negate(x: RationalNumber): RationalNumber =
    RationalNumber(x.r.negate)

  /**
    * Adds two RationalNumber instances and returns their sum as a new RationalNumber.
    *
    * @param x the first RationalNumber operand
    * @param y the second RationalNumber operand
    * @return a new RationalNumber representing the sum of x and y
    */
  def plus(x: RationalNumber, y: RationalNumber): RationalNumber =
    RationalNumber(x.r + y.r)

  /**
    * Divides one `RationalNumber` by another.
    *
    * This method performs the division of two `RationalNumber` instances and
    * returns the resulting `RationalNumber`. It is assumed that division
    * by zero is handled appropriately by the implementation.
    *
    * @param x the dividend, represented as a `RationalNumber`
    * @param y the divisor, represented as a `RationalNumber`
    * @return the result of dividing `x` by `y` as a `RationalNumber`
    */
  def div(x: RationalNumber, y: RationalNumber): RationalNumber =
    RationalNumber(x.r / y.r)

  /**
    * Multiplies two `RationalNumber` instances and returns the result.
    *
    * This method performs the multiplication of two rational numbers, resulting in a new `RationalNumber`
    * that represents their product.
    *
    * @param x the first `RationalNumber` to multiply
    * @param y the second `RationalNumber` to multiply
    * @return a `RationalNumber` that is the product of the two input rational numbers
    */
  def times(x: RationalNumber, y: RationalNumber): RationalNumber =
    RationalNumber(x.r * y.r)

  /**
    * Provides a constant value representing the multiplicative identity in the `RationalNumber` context.
    *
    * This method returns a `RationalNumber` instance corresponding to the value `1`,
    * which serves as the identity element for multiplication operations in the `RationalNumber` algebra.
    *
    * @return a `RationalNumber` instance representing the value `1`.
    */
  def one: RationalNumber = RationalNumber(Rational.one)

  /**
    * Provides the additive identity element for `RationalNumber`.
    *
    * This method returns the zero value for `RationalNumber`, which serves as the
    * identity for addition in the context of rational numbers.
    *
    * @return a `RationalNumber` instance representing zero.
    */
  def zero: RationalNumber = RationalNumber(Rational.zero)

  /**
    * Computes the inverse of the current instance if one exists.
    *
    * This method calculates the inverse of the current object within the context of the implementing
    * algebraic structure. The result is returned as an `Option`, where `None` indicates that the
    * inverse does not exist.
    *
    * @return an `Option` wrapping the inverse of type `T`, or `None` if the inverse does not exist.
    */
  def inverse: Option[RationalNumber] = Some(RationalNumber(r.invert))

  /**
    * Determines if the number is represented exactly without any approximation.
    *
    * @return true if the number is exact, false otherwise
    */
  def isExact: Boolean = true

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = r.isZero

  /**
    * Method to render this Structure in a presentable manner.
    *
    * @return a String
    */
  def render: String = r.render

  /**
    * Adds the specified `T` to this `T` instance.
    *
    * @param t an instance of `T` to be added to this `T`
    * @return a new `T` representing the sum of this `T` and the given `T`
    */
  def +(t: RationalNumber): RationalNumber = plus(this, t)

  /**
    * Computes the additive inverse of this instance.
    *
    * This method returns a new instance representing the negation of this value,
    * as defined in the additive structure of the type `T`.
    *
    * @return a new instance of type `T` that is the additive inverse of this instance
    */
  def unary_- : RationalNumber = negate(this)

  /**
    * Subtracts the specified `RationalNumber` from this `RationalNumber`.
    *
    * This method computes the difference between the current `RationalNumber` instance
    * and the given `RationalNumber` by adding the additive inverse of the operand.
    *
    * @param t the `RationalNumber` to be subtracted from this instance
    * @return a new `RationalNumber` representing the result of the subtraction
    */
  def -(t: RationalNumber): RationalNumber = this + -t

  /**
    * Adds the given `Number` to this `Number` and returns the result.
    *
    * This method performs addition based on the specific type of the input `Number`.
    * - If the input is a `RationalNumber`, it adds the two instances.
    * - If the input is an `Angle`, it converts it to a `FuzzyNumber` with a zero prototype and performs addition.
    * - If the input is a `FuzzyNumber`, it performs addition with the current instance converted to a `FuzzyNumber` with a zero prototype.
    *
    * @param that the `Number` to be added to this instance
    * @return a `Number` representing the result of the addition
    */
  def doPlus(that: Number): Option[Number] = that match {
    case r@RationalNumber(_) => Some(this + r)
    case a@Angle(_) =>
      a.convert(FuzzyNumber.zero).flatMap(this.doPlus)
    case f@FuzzyNumber(_, _) =>
      this.convert(FuzzyNumber.zero).map(x => f plus(x, f))
  }
}

/**
  * Represents a rational number with various arithmetic and algebraic capabilities.
  *
  * The `RationalNumber` class supports mathematical operations such as addition,
  * subtraction, multiplication, and division, while adhering to the mathematical
  * properties of rational numbers. It also provides mechanisms for comparison,
  * negation, inversion, and other numeric operations.
  */
object RationalNumber {
  /**
    * Creates a new `RationalNumber` instance from the given `Rational` value.
    *
    * @param r the `Rational` value to be converted into a `RationalNumber`
    * @return a new `RationalNumber` instance representing the given `Rational`
    */
  def apply(r: Rational): RationalNumber = new RationalNumber(r)

  /**
    * Provides an implicit `Show` instance for `RationalNumber`.
    *
    * This implicit instance utilizes the `render` method of `RationalNumber`
    * to define how instances of `RationalNumber` are represented as a string.
    * It integrates with the `Show` typeclass to allow consistent string representation
    * of rational numbers.
    */
  implicit val showRationalNumber: Show[RationalNumber] = Show.show(_.render)

  /**
    * Returns the additive identity for `RationalNumber`.
    *
    * This method provides the zero value of type `RationalNumber`, which acts as
    * the identity element for addition in the context of rational numbers.
    *
    * @return the zero value as a `RationalNumber` instance.
    */
  def zero: RationalNumber = new RationalNumber(Rational.zero).zero

  /**
    * Provides the multiplicative identity element for `RationalNumber`.
    *
    * This method returns a `RationalNumber` instance equivalent to the value `1`,
    * which serves as the identity element for multiplication in the context of rational numbers.
    *
    * @return a `RationalNumber` instance representing the multiplicative identity `1`.
    */
  def one: RationalNumber = RationalNumber.zero.one
}