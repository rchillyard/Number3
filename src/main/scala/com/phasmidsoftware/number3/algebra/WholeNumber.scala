/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number3.algebra

import cats.Show
import cats.kernel.CommutativeGroup
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import com.phasmidsoftware.number3.algebra.WholeNumber.wholeNumberIsCommutativeGroup
import spire.math.SafeLong

/**
  * A case class representing a whole number.
  *
  * The `WholeNumber` class models any integer in the Z domain and its associated operations,
  * enabling addition, inversion, comparison, and rendering.
  *
  * @param x a SafeLong value representing the whole number
  */
case class WholeNumber(x: SafeLong) extends Additive[WholeNumber] with Number {

  /**
    * Compares the current `WholeNumber` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is a `WholeNumber`, this method compares their underlying SafeLong values.
    * If the provided `Number` is not a `WholeNumber`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `WholeNumber` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `WholeNumber` is less than the provided `WholeNumber`,
    *         `Some(0)` indicates that both WholeNumbers are equal, `Some(1)` indicates that the current `WholeNumber` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Number): Option[Int] = that match {
    case WholeNumber(o) =>
      Some(x.compare(o))
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
  def convert[T <: Number](t: T): Option[T] = t match {
    case _: RationalNumber =>
      Some(RationalNumber(Rational(x.toBigInt)).asInstanceOf[T])
    case _ =>
      None
  }

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = x == 0L

  /**
    * Method to determine if this Structure object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a `PureNumber`, it would no longer be exact.
    *
    * @return true if this `Structure` object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = true

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] =
    Some(x.toDouble)

  /**
    * Renders this `WholeNumber` instance as a string representation of its SafeLong.
    *
    * @return a string representation of the `WholeNumber`
    */
  def render: String = x.toString

  /**
    * Computes the additive inverse of the current `WholeNumber` instance.
    *
    * This method negates the current WholeNumber, returning a new `WholeNumber` instance
    * with the opposite value, relative to `WholeNumber.zero`.
    *
    * @return a new `WholeNumber` instance representing the additive inverse of the current WholeNumber.
    */
  def unary_- : WholeNumber =
    wholeNumberIsCommutativeGroup.inverse(this)

  /**
    * Adds the specified `T` to this `T` instance.
    *
    * @param t an instance of `T` to be added to this `T`
    * @return a new `T` representing the sum of this `T` and the given `T`
    */
  def +(t: WholeNumber): WholeNumber =
    wholeNumberIsCommutativeGroup.combine(this, t)

  /**
    * Subtracts the specified `T` from this `T` instance.
    *
    * @param t an instance of `T` to be subtracted from this `T`
    * @return a new `Additive[T]` representing the result of the subtraction of the given `T` from this `T`
    */
  def -(t: WholeNumber): WholeNumber =
    this + -t

  /**
    * Performs an addition operation between the current `Number` instance and another `Number` instance.
    * Depending on the type of `that`, delegates the operation appropriately.
    *
    * @param that the `Number` instance to add to the current `Number` instance
    * @return a `Number` instance representing the result of the addition
    */
  def doPlus(that: Number): Option[Number] = that match {
    case a: WholeNumber =>
      Some(this + a)
    case x =>
      x doPlus this
  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return Some(PureNumber)
    */
  def maybeFactor: Option[Factor] = Some(PureNumber)
}

/**
  * The `WholeNumber` companion object contains utility methods, predefined constants, and
  * typeclass instances for working with WholeNumbers. WholeNumbers are represented using
  * rational numbers and comply with the algebraic structure of a commutative group.
  */
object WholeNumber {

  /**
    * Represents the additive identity for WholeNumbers.
    *
    * This value denotes zero, serving as the identity element in
    * the group structure of WholeNumbers.
    */
  val zero: WholeNumber = WholeNumber(0L)

  /**
    * Provides an implicit `Show` instance for the `WholeNumber` class, enabling conversion
    * of an `WholeNumber` instance to a string representation using its `render` method.
    *
    * This allows the `WholeNumber` class to integrate seamlessly with libraries or frameworks
    * requiring a `Show` typeclass instance for displaying or logging purposes.
    */
  implicit val showWholeNumber: Show[WholeNumber] = Show.show(_.render)

  /**
    * Provides an implicit implementation of a commutative group for the `WholeNumber` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `WholeNumber` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has an additive inverse.
    */
  implicit object wholeNumberIsCommutativeGroup extends CommutativeGroup[WholeNumber] {
    /**
      * Provides the identity element for the `WholeNumber` group, representing a WholeNumber of zero.
      *
      * @return the `WholeNumber` instance zero.
      */
    def empty: WholeNumber = WholeNumber.zero

    /**
      * Combines two `WholeNumber` instances by adding their respective SafeLong values.
      *
      * @param x the first `WholeNumber` to combine
      * @param y the second `WholeNumber` to combine
      * @return a new `WholeNumber` representing the sum of the SafeLong values of the two provided `WholeNumber` instances
      */
    def combine(x: WholeNumber, y: WholeNumber): WholeNumber =
      WholeNumber(x.x + y.x)

    /**
      * Computes the additive inverse of the given `WholeNumber`.
      *
      * This method negates the input WholeNumber, returning a `WholeNumber` instance
      * that represents its additive inverse, relative to `WholeNumber.zero`.
      *
      * @param x the `WholeNumber` instance to be inverted
      * @return a new `WholeNumber` instance representing the additive inverse of the input
      */
    def inverse(x: WholeNumber): WholeNumber =
      WholeNumber(-x.x)
  }
}
