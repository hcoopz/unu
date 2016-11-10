package unu

import scala.language.experimental.macros

import spire.algebra._

case class Convert[U, A <: Term, B <: Term](factor: U) extends AnyVal {
  def apply(value: Value[U, A])(implicit msemigroup: MultiplicativeSemigroup[U]): Value[U, B] = Value(msemigroup.times(factor, value.value))
}

trait ConvertLowPriority {
  implicit def fromMacro[U, A <: Term, B <: Term]: Convert[U, A, B] = macro Macro.materializeConvert[U, A, B]
}

object Convert extends ConvertLowPriority {
  implicit def default[U, A <: Term](implicit mmonoid: MultiplicativeMonoid[U]): Convert[U, A, A] = new Convert(mmonoid.one)
}
