import unu.number._

import scala.language.implicitConversions

package object unu {
  type _unit = Term.Dimensionless
  type **[A <: Term, B <: Term] = Term.Mult[A, B]
  type /[A <: Term, B <: Term] = Term.Div[A, B]
  type ^[A <: Term, R <: Rational] = Term.Exp[A, R]

  type ~[U, A <: Term] = Value[U, A]

  implicit def valueSyntax[U, A <: Term](value: Value[U, A]): ValueSyntax[U, A] = ValueSyntax(value.value)
}
