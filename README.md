# Number3
Major revision of Number, employing Scala 3

For details of the original version, see <https://github.com/rchillyard/Number.git>.

The purpose of version 2 is to restructure the Number project such that it has a stronger formal relationship with pure mathematics.
Note that this document is a work in progress and will be changed without explicit markers (but Word will track the changes).
Proposed changes
Number3
Currently, development if Number version 2 is being undertaken in the following (temporary) repository: <https://github.com/rchillyard/Number3.git>
Although it is currently built with Scala 2.13.17, the intention is for all the version 2 code to be in Scala 3 (hence the name of the repository).
The new code is built on top of Number version 1.2.12 which includes cats.kernel and typelevel.algebra packages.
Fuzzy Numbers
Fuzzy Numbers will always be based on the Double type. In version 1, we allowed the addition of “fuzz” to any kind of value (Int, Rational, or Double). This follows from the observation that, if a quantity is fuzzy, then double precision is quite sufficient to characterize the nominal value of the quantity.
Generally speaking, the fuzzy types from version 1 will remain unchanged in version 2.
Expressions
All constants will be of type Expression (i.e., lazy). In version 1, it was possible (but not advised) to reference constants that were of the Number type.
Otherwise, the Expression type will be mostly unchanged. The biggest difference will be that the evaluate, evaluateAsIs, and materialize methods that yield a (possibly optional) “Field” (not a good representation of the mathematical concept of field) will instead yield a type, Number.
Number
The version 2 Number type will be (partially) orderable and subtyped by mathematical structures such as group, field, etc.
Examples are RationalNumber, FuzzyNumber, and Angle (see below).
Fields
In version 1, we have a type called Field with subtypes Complex, Real, and Algebraic. However, it does not follow the strict definition of a field and so will be removed and/or replaced.
Format
In Number version 1, numbers are represented as a Value and a Factor. This factor allows us to represent pure number, radians, square roots, logarithms, etc. The plan is to replace this factor mechanism with explicit numeric types, the first of which is Angle representing radians.
RationalNumber
This type extends Number with Field\[RationalNumber] (where Field is algebra.ring.Field).
Angle
Angle extends Group\[Angle] with Number, such that it defines the “circle group” S¹ (-𝛑 to 𝛑). Also known as ℝ/2πℤ., i.e., the real numbers modulo 2π. \[Actually, as of now, it doesn’t extend Number but may later.]

