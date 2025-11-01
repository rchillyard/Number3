package com.phasmidsoftware.number3.algebra

import com.phasmidsoftware.number.core.inner.{Factor, Rational}
import com.phasmidsoftware.number.core.{Constants, ExactNumber, GeneralNumber, NumberExceptionWithCause}
import com.phasmidsoftware.number3.core.{Complex, Structure}
import com.phasmidsoftware.number3.expression.ExpressionFunction.valuableToField
import com.phasmidsoftware.number3.parse.NumberParser

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * A trait representing an object that is in some sense numerical and has a value (or possibly more than one value).
  * `Valuable` does not define an order because a sub-class may not be comparable, for example, a complex number.
  *
  * The properties exposed by this trait are: `isExact`, `approximation`, `maybeDouble`.
  *
  * NOTE: this trait has the same name as the `Valuable` typeclass in the `com.phasmidsoftware.number` package,
  * but it is not the same thing.
  */
trait Valuable {

  /**
    * Method to render this `Valuable` for presentation to the user.
    *
    * @return a String
    */
  def render: String

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
  def isExact: Boolean

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double]
  
  /**
    * Optionally retrieves a factor associated with this `Valuable` if one exists (this is a Scalar).
    *
    * Factors are components or divisors related to the numerical value represented 
    * by this `Valuable`. If no such factor exists or is applicable, the result will 
    * be `None`.
    *
    * @return an `Option` containing the `Factor` if available, otherwise `None`.
    */
  def maybeFactor: Option[Factor]
}

/**
  * Object `Valuable` provides utility methods and implicit conversions related to the `Valuable` trait,
  * enabling parsing and conversion of strings to `Valuable` representations.
  */
object Valuable {
  val zero: Valuable = Number.zero
  val one: Valuable = Number.one
  val minusOne: Valuable = Number.minusOne
  val two: Valuable = Scalar(2)
  val half: Valuable = RationalNumber(Rational.half)
  val pi: Valuable = Angle.pi
  val piBy2: Valuable = Angle.piBy2
  val piBy4: Valuable = Angle.piBy4
  val e: Valuable = NatLog.e
  val infinity: Valuable = RationalNumber.zero.inverse
  val negInfinity: Valuable = RationalNumber(Rational.negInfinity)
  val root2: Valuable = Valuable(Constants.root2)
  val root3: Valuable = Valuable(Constants.root3)
  
  /**
    * Parses the given string into a `Valuable` representation. If the string cannot be parsed
    * into a valid `Number`, an exception is thrown.
    *
    * @param str the input string representing a numerical value.
    * @return a `Valuable` representation of the parsed `Number`.
    * @throws NumberExceptionWithCause if parsing the string fails.
    */
  def apply(str: String): Valuable =
    NumberParser.parseNumber(str) match {
      case Success(number) =>
        Scalar(number)
      case Failure(exception) =>
        throw NumberExceptionWithCause("Valuable.apply", exception)
    }

  def apply(x: Long): Valuable = WholeNumber(x)
  
  /**
    * Creates a `Valuable` instance based on the given `Field`.
    * If the `Field` is a `Real` object, it converts it into a `Scalar` representation.
    * Otherwise, it throws an `IllegalArgumentException`.
    *
    * @param field the input field to be converted into a `Valuable`. It is expected
    *              to be of type `com.phasmidsoftware.number.core.Real`.
    * @return a `Valuable` representation of the input `Field` as a `Scalar`.
    * @throws IllegalArgumentException if the provided `Field` is not of type `Real`.
    */
  def apply(field: com.phasmidsoftware.number.core.Field): Valuable =
    field match {
      case com.phasmidsoftware.number.core.Real(n) =>
        Scalar(n)
        // TODO add other field types such as Complex, Algebraic, etc.
      case _ => throw new IllegalArgumentException(s"Valuable.apply: field is not a Number: $field")
    }
    
  /**
    * Extractor method to convert a `Valuable` instance into an `Option` containing its corresponding `Field` representation.
    * This allows for safe pattern matching and handling of `Valuable` objects that may or may not be convertible to a `Field`.
    *
    * @param v the `Valuable` instance to be converted into an `Option[Field]`
    * @return `Some(Field)` if the conversion is successful, or `None` if it fails
    */
  def unapply(v: Valuable): Option[com.phasmidsoftware.number.core.Field] =
    Try(valuableToField(v)).toOption
    
  /**
    * Converts a given string into a `Valuable` representation.
    * This method allows implicit conversion from `String` to `Valuable`.
    *
    * @param w the input string to be converted into a `Valuable`.
    * @return a `Valuable` instance parsed from the provided string.
    */
  implicit def toValuable(w: String): Valuable = apply(w)
}