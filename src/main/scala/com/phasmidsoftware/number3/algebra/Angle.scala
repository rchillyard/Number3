/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number3.algebra

import algebra.Group
import com.phasmidsoftware.number.core.inner.Value

/**
 * Represents an angle measured in radians.
 *
 * The `Angle` class models an angle as a numeric value in radians, providing
 * operations to combine angles, compute inverses, and access the identity element
 * in the context of group theory.
 *
 * @constructor Creates an `Angle` with the specified value in radians.
 * @param radians the numeric representation of the angle in radians
 *                which is used in mathematical operations.
 */
case class Angle(radians: Number) extends Group[Angle] {
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
   * @param a The angle to be negated.
   * @return A new angle representing the additive inverse of the input angle.
   */
  def inverse(a: Angle): Angle = radians match {
    case RationalNumber(r) => Angle(RationalNumber(r.invert))
    case _ => ??? // TODO implement me
  }

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
    case _ => throw new UnsupportedOperationException("Angle.combine")
  }
}

object Angle {
  def apply(r: RationalNumber): Angle = {
    val value: Value = Value.fromRational(r.r)
    val z: Value = com.phasmidsoftware.number.core.inner.Radian.modulate(value)
    val rational: RationalNumber = z match {
      case Right(x) => RationalNumber(x)
      case Left(Right(x)) => RationalNumber(x)
      case Left(Left(Some(x))) => RationalNumber(x)
      case Left(Left(None)) => RationalNumber.zero // TODO - this should be an error
    }
    new Angle(rational)
  }
}
