package com.phasmidsoftware.number3.algebra

import algebra.ring.Field
import cats.Show
import com.phasmidsoftware.number.core.Fuzziness
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import com.phasmidsoftware.number3.algebra.RationalNumber.rationalNumberIsField

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
    * Determines if the number is represented exactly without any approximation.
    *
    * @return true if the number is exact, false otherwise
    */
  override def isExact: Boolean = true

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a FuzzyNumber--only for exact numbers.
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

  // CONSIDER why doesn't this work with implicitly...?
  private val rf: Field[RationalNumber] = rationalNumberIsField

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

  /**
    * Computes a potential factor for the current `RationalNumber` instance.
    *
    * This method attempts to determine a factor related to the instance and returns it wrapped in an `Option`.
    * If no such factor exists or can be determined, it returns `None`.
    *
    * @return an `Option` containing a `Factor` instance if a factor is defined, or `None` if no factor is applicable or computable.
    */
  def maybeFactor: Option[Factor] = Some(PureNumber)
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
    def zero: RationalNumber = RationalNumber.zero

    /**
      * Retrieves the constant rational number representing one.
      *
      * @return A `RationalNumber` instance equal to one.
      */
    def one: RationalNumber = RationalNumber.one

    /**
      * Adds two rational numbers and returns their sum.
      *
      * @param x the first rational number
      * @param y the second rational number
      * @return the sum of the two rational numbers
      */
    def plus(x: RationalNumber, y: RationalNumber): RationalNumber = RationalNumber(x.r + y.r)

    /**
      * Returns the additive inverse of the given rational number.
      *
      * @param x the rational number to negate
      * @return a rational number representing the additive inverse of the input
      */
    def negate(x: RationalNumber): RationalNumber = RationalNumber(-x.r)

    /**
      * Divides one rational number by another.
      *
      * @param x the numerator rational number
      * @param y the denominator rational number
      * @return a new RationalNumber representing the result of the division
      */
    def div(x: RationalNumber, y: RationalNumber): RationalNumber = RationalNumber(x.r / y.r)

    /**
      * Multiplies two rational numbers.
      *
      * @param x the first rational number
      * @param y the second rational number
      * @return the product of the two rational numbers
      */
    def times(x: RationalNumber, y: RationalNumber): RationalNumber = RationalNumber(x.r * y.r)
  }
}
