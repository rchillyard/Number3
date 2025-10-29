package com.phasmidsoftware.number3.algebra

import algebra.ring.Field
import cats.Show
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import com.phasmidsoftware.number3.algebra.RationalNumber.rationalNumberIsField
import com.phasmidsoftware.number3.algebra.Real.realIsRing
import com.phasmidsoftware.number3.core.Structure

/**
  * Represents a rational number and provides arithmetic operations
  * along with functions to retrieve its identity and inverse elements.
  *
  * This class extends `Number` and `Field` to provide additional
  * operations specific to rational numbers.
  *
  * A "field" notated (F, +, x), is formed from two abelian (commutative) groups:
  * - the additive group (F, +) in which every element has an (additive) inverse;
  * - the multiplicative group (F*, x) in which every nonzero element has a (multiplicative) inverse.
  * Note that the multiplicative group does not include the zero element (zero is the identity of the
  * additive group but plays no part in the multiplicative group.
  *
  * `RationalNumber`, however, is a field that includes both zero and infinity so that it is a complete field.
  *
  * @constructor Creates a new RationalNumber instance with the given
  *              rational value `r`.
  * @param r the underlying rational value
  */
case class RationalNumber(r: Rational) extends Additive[RationalNumber] with Multiplicative[RationalNumber] with Number {
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
  def compareExact(that: Scalar): Option[Int] = that match {
    case RationalNumber(o) =>
      Some(r.compareTo(o))
    case WholeNumber(x) =>
      Some(r.compare(Rational(x.toBigInt)))
    case _ =>
      None
  }

  /**
    * Converts the given number to an instance of the specified type, if possible.
    *
    * The method attempts to convert the input into a `Real` if it matches the specific type constraint,
    * or returns `None` otherwise.
    *
    * @param t a prototype of the required output.
    * @tparam T the type of the number, which must be a subtype of `Number`
    * @return an `Option` containing the converted value of type `T` if successful, or `None` if the conversion is not possible
    */
  def convert[T <: Structure](t: T): Option[T] = t match {
    case _: Real =>
      Some(Real(r.toDouble, None).asInstanceOf[T])
    case _: WholeNumber =>
      Option.when(r.isWhole)(WholeNumber(r.toBigInt).asInstanceOf[T])
    case _ =>
      None
  }

  /**
    * Determines if the number is represented exactly without any approximation.
    *
    * @return true if the number is exact, false otherwise
    */
  override def isExact: Boolean = true

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] = r.maybeDouble

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = r.isZero

  /**
    * Method to render this RationalNumber for presentable.
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
  def +(t: RationalNumber): RationalNumber =
    rf.plus(this, t) // CONSIDER why doesn't this work with implicitly...?

  /**
    * Computes the additive inverse of this instance.
    *
    * This method returns a new instance representing the negation of this value,
    * as defined in the additive structure of the type `T`.
    *
    * @return a new instance of type `T` that is the additive inverse of this instance
    */
  def unary_- : RationalNumber =
    rf.negate(this)

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
    * Multiplies the specified `T` by this `T` instance.
    *
    * @param t an instance of `T` to be multiplied by this `T`
    * @return a new `Multiplicative[T]` representing the product of this `T` and the given `T`
    */
  def *(t: RationalNumber): Multiplicative[RationalNumber] = rf.times(this, t)

  /**
    * Divides this `T` instance by the specified `T`.
    *
    * @param t an instance of `T` to be the divisor
    * @return a new `Multiplicative[T]` representing the quotient of this `T` and `t`
    */
  def /(t: RationalNumber): Multiplicative[RationalNumber] = rf.div(this, t)

  /**
    * Adds the given `Scalar` to this `Scalar` and returns the result as an `Option[Scalar]`.
    *
    * The addition is performed based on the type of the input `Scalar`. If the input is:
    * - A `RationalNumber`, the result is computed by adding it to this instance.
    * - An `Angle`, the angle is converted to a `Real` to determine compatibility for addition.
    * - A `Real`, an attempt is made to convert this instance to a compatible `Real` and perform the operation.
    *
    * @param that the `Scalar` to be added to the current instance
    * @return an `Option[Scalar]` containing the result of the addition, or `None` if the operation is not valid
    */
  def doPlus(that: Scalar): Option[Scalar] = that match {
    case r@RationalNumber(_) => Some(this + r)
    case a@Angle(_) =>
      a.convert(Real.zero).flatMap(this.doPlus)
    case f@Real(_, _) =>
      this.convert(Real.zero).map(x => realIsRing.plus(x, f))
  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return Some(PureNumber).
    */
  def maybeFactor: Option[Factor] = Some(PureNumber)

  // CONSIDER why doesn't this work with implicitly...?
  private val rf: Field[RationalNumber] = rationalNumberIsField
}

/**
  * Represents a rational number with basic arithmetic operations and field properties.
  * This class provides methods and implicit instances for managing rational numbers
  * and their integration with mathematical abstractions such as the `Field` typeclass.
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
    * Creates a `RationalNumber` instance from two `Long` values representing the numerator and denominator.
    *
    * @param x the numerator of the rational number
    * @param y the denominator of the rational number
    * @return a new `RationalNumber` instance representing the fraction x / y
    */
  def apply(x: Long, y: Long): RationalNumber = RationalNumber(Rational(x, y))

  /**
    * Creates a new `RationalNumber` instance from the given `Long` value.
    *
    * This method converts the specified `Long` value into a `RationalNumber` representation.
    *
    * @param x the `Long` value to be converted into a `RationalNumber`
    * @return a new `RationalNumber` instance constructed from the given `Long` value.
    */
  def apply(x: Long): RationalNumber = RationalNumber(Rational(x))

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
  def zero: RationalNumber =
    rationalNumberIsField.zero

  /**
    * Provides the multiplicative identity element for `RationalNumber`.
    *
    * This method returns a `RationalNumber` instance equivalent to the value `1`,
    * which serves as the identity element for multiplication in the context of rational numbers.
    *
    * @return a `RationalNumber` instance representing the multiplicative identity `1`.
    */
  def one: RationalNumber =
    rationalNumberIsField.one

  /**
    * Provides an implicit implementation of the `Field` type class for the `RationalNumber` type.
    *
    * This object defines the standard operations required for `RationalNumber`
    * to function as a field, including addition, multiplication, division,
    * and their respective identity and inverse operations. By extending
    * the `Field` type class, it ensures compliance with field axioms.
    */
  implicit object rationalNumberIsField extends Field[RationalNumber] {
    /**
      * Returns the zero value of a `RationalNumber`.
      *
      * @return the zero value of the `RationalNumber` type
      */
    def zero: RationalNumber =
      RationalNumber(Rational.zero)

    /**
      * Retrieves the constant rational number representing one.
      *
      * @return A `RationalNumber` instance equal to one.
      */
    def one: RationalNumber =
      RationalNumber(Rational.one)

    /**
      * Adds two rational numbers and returns their sum.
      *
      * @param x the first rational number
      * @param y the second rational number
      * @return the sum of the two rational numbers
      */
    def plus(x: RationalNumber, y: RationalNumber): RationalNumber =
      RationalNumber(x.r + y.r)

    /**
      * Returns the additive inverse of the given rational number.
      *
      * @param x the rational number to negate
      * @return a rational number representing the additive inverse of the input
      */
    def negate(x: RationalNumber): RationalNumber =
      RationalNumber(-x.r)

    /**
      * Divides one rational number by another.
      *
      * @param x the numerator rational number
      * @param y the denominator rational number
      * @return a new RationalNumber representing the result of the division
      */
    def div(x: RationalNumber, y: RationalNumber): RationalNumber =
      RationalNumber(x.r / y.r)

    /**
      * Multiplies two rational numbers.
      *
      * @param x the first rational number
      * @param y the second rational number
      * @return the product of the two rational numbers
      */
    def times(x: RationalNumber, y: RationalNumber): RationalNumber =
      RationalNumber(x.r * y.r)
  }
}
