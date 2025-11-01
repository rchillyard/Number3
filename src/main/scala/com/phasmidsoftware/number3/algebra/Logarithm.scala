/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number3.algebra

import algebra.CommutativeMonoid
import cats.Show
import cats.kernel.CommutativeGroup
import com.phasmidsoftware.number.core.NumberException
import com.phasmidsoftware.number.core.inner.{Factor, Radian, Rational, Value}
import com.phasmidsoftware.number3.algebra.Logarithm.LogarithmIsCommutativeMonoid
import com.phasmidsoftware.number3.core.Structure
import com.phasmidsoftware.number3.misc.FP

import scala.reflect.ClassTag

/**
  * A case class representing a Logarithm. This class implements the `Additive` and `Radians` traits,
  * allowing operations such as addition, subtraction, and various type conversions.
  *
  * @param x the value of the Logarithm.
  */
abstract class Logarithm(val x: Number) extends Additive[Logarithm] with Transformed with CanAdd[Logarithm] with Ordered[Logarithm] {

  def base: Number

  /**
    * Compares this `Logarithm` instance with another `Logarithm` instance.
    *
    * This method compares the `x` values of the current `Logarithm` instance 
    * and the specified `Logarithm` instance using their natural order.
    *
    * @param that the `Logarithm` instance to compare with the current instance
    * @return an integer value:
    *         - a negative value if this `Logarithm` is less than `that`
    *         - zero if this `Logarithm` is equal to `that`
    *         - a positive value if this `Logarithm` is greater than `that`
    */
  def compare(that: Logarithm): Int = 
    x.compare(that.x)

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
  def convert[T <: Structure: ClassTag](t: T): Option[T] = t match {
    case _: Real =>
      transformation
    case _ =>
      None
  }

  /**
    * NOTE that adding logarithms is the equivalent of multiplying the corresponding (exponential) values.
    *
    * Adds the specified `Scalar` to the current `Scalar` and returns the result as an `Option[Scalar]`.
    * This method handles addition based on the type of `Scalar` provided. If the input is an `Logarithm`,
    * it computes the sum of the current `Logarithm` and the provided `Logarithm`.
    *
    * @param that the `Scalar` to be added to the current instance
    * @return an `Option[Scalar]` containing the result of the addition, or `None` if the operation is not valid
    */
  def doPlus(that: Logarithm): Option[Logarithm] =
    if (getClass == that.getClass)
      Some(this + that)
    else
      None

  /**
    * Scales the instance of type T by the given integer multiplier.
    *
    * This method performs a multiplication operation between the current instance and
    * the specified integer, returning an optional result. The result is defined if
    * the scaling operation is valid for the specific implementation.
    *
    * @param that the integer multiplier used to scale the instance
    * @return an Option containing the scaled result of type T, or None if the operation is invalid
    */
  def doScaleInt(that: Int): Option[Monotone] = ???

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = x.isZero

  /**
    * Determines the sign of the scalar value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int = x.compareExact(WholeNumber.zero).get

  /**
    * Method to determine if this Structure object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this Structure object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = x.isExact

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
    * Renders this `Logarithm` instance as a string representation of x in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Logarithm` in terms of π
    */
  def render: String = {
    val prefix = x.render
    val suffix = "𝛑"
    (if prefix == "1" then "" else prefix) + suffix
  }

  /**
    * Computes the additive inverse of the current `Logarithm` instance.
    *
    * This method negates the current Logarithm, returning a new `Logarithm` instance
    * with the opposite value, relative to `Logarithm.zero`.
    *
    * @return a new `Logarithm` instance representing the additive inverse of the current Logarithm.
    */
  def unary_- : Logarithm =
    throw new UnsupportedOperationException("Logarithm.unary_-")

  /**
    * Adds the specified `Logarithm` to the current `Logarithm` instance.
    *
    * This method combines the current Logarithm with the provided Logarithm
    * by adding their respective x, returning a new `Logarithm` instance
    * representing the sum.
    *
    * @param a the `Logarithm` to be added to the current `Logarithm`
    * @return a new `Logarithm` representing the sum of the current `Logarithm` and the specified `Logarithm`
    */
  def +(a: Logarithm): Logarithm =
    LogarithmIsCommutativeMonoid.combine(this, a)

  /**
    * Subtracts the specified `Logarithm` from the current `Logarithm` instance.
    *
    * This method computes the difference by adding the additive inverse
    * of the specified `Logarithm` to the current `Logarithm`, effectively implementing subtraction.
    *
    * @param a the `Logarithm` to subtract from the current `Logarithm`
    * @return an instance of `Additive[Logarithm]` representing the result of the subtraction
    */
  def -(a: Logarithm): Additive[Logarithm] = this + -a

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
  * A case class representing a Logarithm. This class implements the `Additive` and `Radians` traits,
  * allowing operations such as addition, subtraction, and various type conversions.
  * Logarithm represents the "circle group," which is a compact Abelian (commutative) group under Logarithm addition,
  * where the addition wraps around the circle.
  * It is compact in that it is bounded by -𝛑 and 𝛑.
  *
  * Logarithm does not support ordering or comparison.
  *
  * @param x the value of the Logarithm.
  */
case class NatLog(x: Number) extends Logarithm(x) {

  val base: Number = Real(math.E)
  
  /**
    * Represents the additive identity element for the type `T`.
    *
    * The additive identity, commonly referred to as "zero," is the element in an
    * additive algebraic structure that, when added to any element of the structure,
    * results in the same element. For any element `x`, `x + zero` and `zero + x` should
    * equal `x`.
    */
  val zero: Logarithm = NatLog(WholeNumber.zero)

  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value. 
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    */
  def transformation[T: ClassTag]: Option[T] = {
    val c = implicitly[ClassTag[T]]
    if (c.runtimeClass == classOf[Real]) {
      val result: Real = 
      x match {
        case WholeNumber.one | RationalNumber(Rational.one) => 
          Real(math.E)
        case Real(value, fuzz) =>
          Real(math.log(value), fuzz)
        case _ => 
          ???
      }
      Some(result.asInstanceOf[T])
    } 
    else
      None
  }

  /**
    * Compares the current `Logarithm` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is an `Logarithm`, this method compares their underlying radian values.
    * If the provided `Number` is not an `Logarithm`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `Logarithm` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `Logarithm` is less than the provided `Logarithm`,
    *         `Some(0)` indicates that both Logarithms are equal, `Some(1)` indicates that the current `Logarithm` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Number): Option[Int] = that match {
    case NatLog(r) =>
      Some(x.compare(r))
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
  def convert[T <: Structure: ClassTag](t: T): Option[T] = t match {
    case _: Real =>
      x.approximation.map(x => x.scaleFactor).asInstanceOf[Option[T]]
    case _ =>
      None
  }

  /**
    * Renders this `Logarithm` instance as a string representation of x in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Logarithm` in terms of π
    */
  def render: String = {
    val number = x.render
    "e" + (if (number == "1") "" else s"^$number")
  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return an `Option` containing a `Factor` if available, otherwise `None`
    */
  def maybeFactor: Option[Factor] = Some(com.phasmidsoftware.number.core.inner.NatLog)
}

object NatLog {

  /**
    * Represents the zero value of the `Logarithm` class.
    *
    * This is a predefined constant which corresponds to an `Logarithm` of zero x.
    * It is used as the additive identity in operations involving Logarithms.
    */
  val one: Logarithm = NatLog(WholeNumber.zero)

  /**
    * Represents the natural logarithmic base `e` as a `Logarithm` instance.
    *
    * The value corresponds to the natural logarithm of one in terms of base `e`,
    * often used as a mathematical constant in logarithmic and exponential calculations.
    */
  val e: Logarithm = NatLog(WholeNumber.one)
}

/**
  * The `Logarithm` companion object contains utility methods, predefined constants, and
  * typeclass instances for working with Logarithms. Logarithms are represented using
  * rational numbers and comply with the algebraic structure of a commutative group.
  */
object Logarithm {

  /**
    * Provides an implicit `Show` instance for the `Logarithm` class, enabling conversion
    * of an `Logarithm` instance to a string representation using its `render` method.
    *
    * This allows the `Logarithm` class to integrate seamlessly with libraries or frameworks
    * requiring a `Show` typeclass instance for displaying or logging purposes.
    */
  implicit val showLogarithm: Show[Logarithm] = Show.show(_.render)

  /**
    * Provides an implicit implementation of a commutative group for the `Logarithm` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `Logarithm` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has an additive inverse.
    */
  implicit object LogarithmIsCommutativeMonoid extends CommutativeMonoid[Logarithm] {
    /**
      * Provides the identity element for the `Logarithm` group, representing an Logarithm of zero x.
      *
      * @return an `Logarithm` instance with zero x, acting as the identity element in the group structure.
      */
    def empty: Logarithm = NatLog.one

    /**
      * Combines two `Logarithm` instances by adding their respective x.
      *
      * @param x the first `Logarithm` to combine
      * @param y the second `Logarithm` to combine
      * @return a new `Logarithm` representing the sum of the x of the two provided `Logarithm` instances
      */
    def combine(x: Logarithm, y: Logarithm): Logarithm = (x, y) match {
      case (a, b) if a.getClass == b.getClass =>
        a + b
      case _ =>
        throw NumberException(s"Logarithm.combine: $x, $y")
    }
  }
}
