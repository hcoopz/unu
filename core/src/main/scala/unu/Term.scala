package unu

import spire.algebra._
import spire.math._

sealed trait Term extends Product with Serializable
object Term {
  sealed trait Dimensionless extends Term
  sealed trait Exp[A <: Term, R <: unu.number.Rational] extends Term
  sealed trait Mult[A <: Term, B <: Term] extends Term
  sealed trait Div[A <: Term, B <: Term] extends Term
  case class BaseUnit() extends Term
  case class DerivedUnit[A <: Term](num: Int, denom: Int) extends Term {
    @inline def ratio[U](implicit mgroup: MultiplicativeGroup[U], fractional: Fractional[U]): U = mgroup.div(fractional.fromInt(num), fractional.fromInt(denom))
  }
}
