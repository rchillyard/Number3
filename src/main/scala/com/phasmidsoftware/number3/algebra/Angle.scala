/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number3.algebra

import cats.Show
import cats.kernel.CommutativeGroup
import com.phasmidsoftware.number.core.inner.Rational.convertDouble
import com.phasmidsoftware.number.core.inner.{Radian, Rational, Value}

/**
 * A case class representing an angle in radians.
 *
 * The `Angle` class models an angle and its associated operations,
 * enabling addition, inversion, comparison, and rendering of angles.
 * An `Angle` is expressed in terms of radians and supports exactness
 * checks, conversions, and a variety of mathematical operations.
 *
 * @param radians a non-Angle numerical value representing the angle in radians
 */
case class Angle(radians: Number) extends Additive[Angle] with CommutativeGroup[Angle] with Number {

  require(!radians.isInstanceOf[Angle], "Angle must not be based on an Angle")

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
  def compareExact(that: Number): Int = that match {
    case Angle(o) =>
      radians.compare(o)
    case _ =>
      throw new UnsupportedOperationException(s"Angle.compareExact: $this, $that")
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
  def convert[T <: Number](t: T): Option[T] = t match {
    case _: FuzzyNumber =>
      radians.approximation.asInstanceOf[Option[T]]
    case _ =>
      None
  }

  /**
   * Determines if the current number is equal to zero.
   *
   * @return true if the number is zero, false otherwise
   */
  def isZero: Boolean = radians.isZero

  /**
   * Method to determine if this NumberLike object is exact.
   * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
   *
   * @return true if this NumberLike object is exact in the context of No factor, else false.
   */
  def isExact: Boolean = radians.isExact

  /**
   * Method to render this NumberLike in a presentable manner.
   *
   * @return a String
   */
  def render: String = {
    val prefix = radians.render
    val suffix = "𝛑"
    (if (prefix == "1") "" else prefix) + suffix
  }

  /**
   * Provides the identity element for the `Angle` group, representing an angle of zero radians.
   *
   * This method returns an `Angle` instance corresponding to a zero value,
   * which serves as the identity for the `combine` operation in the group structure.
   *
   * @return an `Angle` instance with zero radians.
   */
  def empty: Angle = Angle(RationalNumber.zero)

  /**
   * Computes the additive inverse of the given angle.
   *
   * @param a a prototype of the result.
   * @return A new angle representing the additive inverse of the input angle.
   */
  def inverse(a: Angle): Angle = radians match {
    case RationalNumber(r) =>
      Angle(RationalNumber(r.negate))
    case FuzzyNumber(x, f) =>
      Angle(FuzzyNumber(-x, f))
  }

  /**
   * Computes the additive inverse of the current `Angle` instance.
   *
   * This method negates the current angle, returning a new `Angle` instance
   * with the opposite value, relative to `Angle.zero`.
   *
   * @return a new `Angle` instance representing the additive inverse of the current angle.
   */
  def unary_- : Angle =
    this.inverse(Angle.zero)

  /**
   * Combines two `Angle` instances by adding their respective radians.
   *
   * @param x the first `Angle` to combine
   * @param y the second `Angle` to combine
   * @return a new `Angle` representing the sum of the radians of the two provided `Angle` instances
   */
  def combine(x: Angle, y: Angle): Angle = (x, y) match {
    case (Angle(x1@RationalNumber(_)), Angle(x2@RationalNumber(_))) =>
      Angle(RationalNumber.zero plus(x1, x2))
    case _ =>
      throw new UnsupportedOperationException("Angle.combine")
  }

  /**
   * Adds the specified `Angle` to the current `Angle` instance.
   *
   * This method combines the current angle with the provided angle
   * by adding their respective radians, returning a new `Angle` instance
   * representing the sum.
   *
   * @param a the `Angle` to be added to the current `Angle`
   * @return a new `Angle` representing the sum of the current `Angle` and the specified `Angle`
   */
  def +(a: Angle): Angle =
    combine(this, a)

  /**
   * Subtracts the specified `Angle` from the current `Angle` instance.
   *
   * This method computes the difference by adding the additive inverse
   * of the specified `Angle` to the current `Angle`, effectively implementing subtraction.
   *
   * @param a the `Angle` to subtract from the current `Angle`
   * @return an instance of `Additive[Angle]` representing the result of the subtraction
   */
  def -(a: Angle): Additive[Angle] = this + -a

  /**
   * Performs an addition operation between the current `Number` instance and another `Number` instance.
   * Depending on the type of `that`, delegates the operation appropriately.
   *
   * @param that the `Number` instance to add to the current `Number` instance
   * @return a `Number` instance representing the result of the addition
   */
  def doPlus(that: Number): Option[Number] = that match {
    case a: Angle =>
      Some(this + a)
    case x =>
      x doPlus this
  }
}

/**
 * Companion object for the `Angle` class, providing factory methods, constants,
 * and utility functionalities related to angles.
 */
object Angle {
  /**
   * Converts the given rational number to an `Angle` instance by performing modulation and necessary computations.
   *
   * @param r the input `RationalNumber` representing the rational value to be converted into an angle
   * @return an `Angle` instance corresponding to the given rational value
   */
  def apply(r: RationalNumber): Angle =
    new Angle(
      Radian.modulate(Value.fromRational(r.r)) match {
        case Right(x) => RationalNumber(x)
        case Left(Right(x)) => RationalNumber(x)
        case Left(Left(Some(x))) => RationalNumber(x)
        case Left(Left(None)) => RationalNumber.zero // TODO - this should be an error
      }
    )

  /**
   * Represents the additive identity for angles.
   *
   * This value denotes an angle of zero radians, serving as the identity element in
   * the group structure of angles. It is constructed using the `Angle` companion object
   * initialized with the additive identity of `RationalNumber`.
   */
  val zero: Angle = Angle(RationalNumber.zero)

  /**
   * Represents an angle equivalent to mathematical π radians.
   *
   * The value `pi` is an instance of the `Angle` class initialized with the
   * `RationalNumber.one`, which corresponds to the rational representation of π
   * in the specific context of the `Angle` implementation.
   */
  val pi: Angle = Angle(RationalNumber.one)

  /**
   * Alias for the `pi` value, representing an angle equivalent to mathematical π radians.
   *
   * This value is a symbolic representation of π radians, reused from the `Angle.pi` value.
   * It is denoted by the Greek mathematical symbol 𝛑 and can be used interchangeably with `pi`.
   */
  val 𝛑: Angle = pi

  /**
   * Represents an angle equivalent to π/2 radians.
   *
   * `pi_2` is a constant instance of the `Angle` class initialized using
   * a `RationalNumber` constructed with a value of 1/2. This corresponds
   * to π/2 radians in mathematical terms.
   */
  val pi_2: Angle = Angle(RationalNumber(Rational.half))

  /**
   * Provides an implicit `Show` instance for the `Angle` class, enabling conversion
   * of an `Angle` instance to a string representation using its `render` method.
   *
   * This allows the `Angle` class to integrate seamlessly with libraries or frameworks
   * requiring a `Show` typeclass instance for displaying or logging purposes.
   */
  implicit val showAngle: Show[Angle] = Show.show(_.render)
}
