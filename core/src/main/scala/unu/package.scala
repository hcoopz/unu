import unu.number._

import scala.language.implicitConversions
import scala.language.experimental.macros

package object unu {
  type _unit = Term.Dimensionless
  type *[A <: Term, B <: Term] = Term.Mult[A, B]
  type /[A <: Term, B <: Term] = Term.Div[A, B]
  type ^[A <: Term, R <: Rational] = Term.Exp[A, R]

  type ~[U, A <: Term] = Value[U, A]

  def value[A <: Term](value: Any): Any = macro Macro.createValue[A]
}
