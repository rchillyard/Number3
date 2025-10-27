package com.phasmidsoftware.number3.algebra

import com.phasmidsoftware.number.core.inner.Factor

/**
  * A trait representing an object that is in some sense numerical and has a value (or possibly more than one value).
  * Valuable does not define an order because sub-class may not be comparable,
  * for example, a complex number.
  *
  * The properties exposed by this trait are: isExact, approximation, maybeDouble.
  *
  * NOTE: this trait has the same name as the `Valuable` typeclass in the Scala library,
  * but it is not the same thing.
  */
trait Valuable {

  /**
    * Method to render this Valuable for presentation to the user.
    *
    * @return a String
    */
  def render: String

  /**
    * Yields an approximation of this `Valuable` object, if applicable.
    *
    * This method attempts to compute an approximate representation of the number
    * in the form of a `FuzzyNumber`, which encapsulates uncertainty or imprecision
    * in its value. If no meaningful approximation is possible for the number, it
    * returns `None`.
    *
    * @return an `Option[FuzzyNumber]` containing the approximate representation
    *         of the number, or `None` if no approximation is available.
    */
  def approximation: Option[FuzzyNumber]

  /**
    * Determines whether this `Valuable` is exact, i.e., has no approximation.
    *
    * CONSIDER it may be possible that there are non-approximatable entities that are not exact either.
    *
    * The method returns `true` if there is no approximate representation
    * available (i.e., `approximation` is `None`), indicating that the
    * entity is exact. Otherwise, it returns `false`.
    *
    * @return a `Boolean` indicating whether the entity is exact (`true`)
    *         or has an approximation (`false`).
    */
  def isExact: Boolean = approximation.isEmpty

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a FuzzyNumber--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double]

  /**
    * Attempts to yield a factor for the instance, if available.
    *
    * A `Factor` is a representation of the underlying numerical domain, for example, `PureNumber`, `Radian`, etc.
    *
    * @return an `Option[Factor]` containing the factor representation of this object,
    *         or `None` if factorization is not applicable or unavailable.
    */
  def maybeFactor: Option[Factor]
}
