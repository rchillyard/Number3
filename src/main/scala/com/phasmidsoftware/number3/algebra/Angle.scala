/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number3.algebra

import cats.Show
import cats.kernel.CommutativeGroup
import com.phasmidsoftware.number.core.inner.{Factor, Radian, Rational, Value}
import com.phasmidsoftware.number3.algebra.Angle.angleIsCommutativeGroup
import com.phasmidsoftware.number3.core.Structure
import com.phasmidsoftware.number3.misc.FP

/**
  * A case class representing an angle in radians.
  *
  * The `Angle` class models an angle and its associated operations,
  * enabling addition, inversion, comparison, and rendering of angles.
  * An `Angle` is expressed in terms of radians and supports exactness
  * checks, conversions, and a variety of mathematical operations.
  *
  * Angle represents the "circle group," which is a compact Abelian (commutative) group under angle addition,
  * where the addition wraps around the circle.
  * It is compact in that it is bounded by -𝛑 and 𝛑.
  *
  * @param radians a non-Angle numerical value representing the angle in radians
  */
case class Angle(radians: Number) extends Additive[Angle] with Radians {

  require(!radians.isInstanceOf[Angle], "Angle must not be based on an Angle")

  /**
    * Compares the current `Angle` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is an `Angle`, this method compares their underlying radian values.
    * If the provided `Number` is not an `Angle`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `Angle` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `Angle` is less than the provided `Angle`,
    *         `Some(0)` indicates that both angles are equal, `Some(1)` indicates that the current `Angle` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Scalar): Option[Int] = that match {
    case Angle(r) =>
      Some(radians.compare(r))
    case _ =>
      None
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
  def convert[T <: Structure](t: T): Option[T] = t match {
    case _: Real =>
      radians.approximation.map(x => x.scaleByPi).asInstanceOf[Option[T]]
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
    * Method to determine if this Structure object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this Structure object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = radians.isExact

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] =
    FP.whenever(isExact)(convert(Real.zero) map (_.value))

  /**
    * Renders this `Angle` instance as a string representation of radians in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Angle` in terms of π
    */
  def render: String = {
    val prefix = radians.render
    val suffix = "𝛑"
    (if (prefix == "1") "" else prefix) + suffix
  }

  /**
    * Computes the additive inverse of the current `Angle` instance.
    *
    * This method negates the current angle, returning a new `Angle` instance
    * with the opposite value, relative to `Angle.zero`.
    *
    * @return a new `Angle` instance representing the additive inverse of the current angle.
    */
  def unary_- : Angle = {
    angleIsCommutativeGroup.inverse(this)
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
    angleIsCommutativeGroup.combine(this, a)

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
    * Adds the specified `Scalar` to the current `Scalar` and returns the result as an `Option[Scalar]`.
    * This method handles addition based on the type of `Scalar` provided. If the input is an `Angle`,
    * it computes the sum of the current `Angle` and the provided `Angle`. If the input is a `Number`,
    * the addition is delegated to the `doPlus` implementation of the `Number`.
    *
    * @param that the `Scalar` to be added to the current instance
    * @return an `Option[Scalar]` containing the result of the addition, or `None` if the operation is not valid
    */
  def doPlus(that: Scalar): Option[Scalar] = that match {
    case a: Angle =>
      Some(this + a)
    case x: Number =>
      x doPlus this
  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return an `Option` containing a `Factor` if available, otherwise `None`
    */
  def maybeFactor: Option[Factor] = Some(Radian)

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
  def approximation: Option[Real] = convert(Real.zero)

}

/**
  * The `Angle` companion object contains utility methods, predefined constants, and
  * typeclass instances for working with angles. Angles are represented using
  * rational numbers and comply with the algebraic structure of a commutative group.
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
    * The value `pi_2` is a constant instance of the `Angle` class initialized using
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

  /**
    * Provides an implicit implementation of a commutative group for the `Angle` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `Angle` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has an additive inverse.
    */
  implicit object angleIsCommutativeGroup extends CommutativeGroup[Angle] {
    /**
      * Provides the identity element for the `Angle` group, representing an angle of zero radians.
      *
      * @return an `Angle` instance with zero radians, acting as the identity element in the group structure.
      */
    def empty: Angle = Angle.zero

    /**
      * Combines two `Angle` instances by adding their respective radians.
      *
      * @param x the first `Angle` to combine
      * @param y the second `Angle` to combine
      * @return a new `Angle` representing the sum of the radians of the two provided `Angle` instances
      */
    def combine(x: Angle, y: Angle): Angle = (x, y) match {
      case (Angle(x1@RationalNumber(_)), Angle(x2@RationalNumber(_))) =>
        Angle(RationalNumber(x1.r + x2.r))
      case _ =>
        throw new UnsupportedOperationException("Angle.combine")
    }

    /**
      * Computes the additive inverse of the given `Angle`.
      *
      * This method negates the input angle, returning an `Angle` instance
      * that represents its additive inverse, relative to `Angle.zero`.
      *
      * @param a the `Angle` instance to be inverted
      * @return a new `Angle` instance representing the additive inverse of the input
      */
    def inverse(a: Angle): Angle = a.radians match {
      case RationalNumber(r) =>
        Angle(RationalNumber(r.negate))
      case Real(x, f) =>
        Angle(Real(-x, f))
    }
  }
}
