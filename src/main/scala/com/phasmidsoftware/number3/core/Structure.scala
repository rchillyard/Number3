/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number3.core

import com.phasmidsoftware.number3.algebra
import com.phasmidsoftware.number3.algebra.FuzzyNumber

/**
  * Represents an Algebraic Structure.
  * In common parlance, we might call such an object, a "number" or "quantity" or a mathematical thing.
  * A `Structure` supports functionality such as exactness evaluation, numeric conversion,
  * rendering, and set membership analysis.
  * In general, we cannot order `Structure` objects, but we can test them for exactness.
  */
trait Structure extends com.phasmidsoftware.number3.algebra.Numeric {

  /**
    * Method to determine if this Structure object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this Structure object is exact in the context of No factor, else false.
    */
  def isExact: Boolean

  /**
    * Converts this `Structure` object into an optional `java.lang.Number` provided that the conversion can be
    * performed without loss of precision.
    *
    * The method determines whether the current `Structure` object can be represented as a `java.lang.Number`
    * by leveraging the `asNumber` method and further evaluating certain conditions:
    * - If the `Structure` object is an `ExactNumber` and its factor is `PureNumber`, the result
    * is converted using `Value.asJavaNumber`.
    * - If the `Structure` object is a `FuzzyNumber` with a `wiggle` value below a specified tolerance,
    * the result is also converted using `Value.asJavaNumber`.
    * - In all other cases, `None` is returned.
    *
    * @return an optional `java.lang.Number` representation of this object. The result is `Some(java.lang.Number)`
    *         if the conversion is successful under the stated conditions; otherwise, `None`.
    */
  def asJavaNumber: Option[java.lang.Number] = this match {
    case algebra.Angle(number) => number.convert(FuzzyNumber.zero).flatMap(x => x.asJavaNumber)
    case algebra.FuzzyNumber(value, _) => Some(value)
    case algebra.RationalNumber(r) => Some(r.toDouble)
    case _ => throw new UnsupportedOperationException(s"asJavaNumber: $this")
  }

  /**
    * Method to render this Structure for presentation to the user.
    *
    * @return a String
    */
  def render: String

  /**
    * Method to determine the NumberSet, if any, to which this Structure object belongs.
    * NOTE that we don't yet support H, the quaternions.
    *
    * @return Some(numberSet) or None if it doesn't belong to any (for example, it is fuzzy).
    */
  def memberOf: Option[NumberSet] =
    Seq(C, R, Q, Z, N).find(set => set.isMember(this))

  /**
    * Method to determine if this Structure object is a member of the given set.
    *
    * @param set the candidate NumberSet.
    * @return true if this is exact and belongs to set.
    */
  def memberOf(set: NumberSet): Boolean = set.isMember(this)
}
