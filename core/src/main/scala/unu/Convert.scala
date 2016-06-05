package unu

import scala.language.experimental.macros

case class Convert[U, A <: Term, B <: Term](factor: U) extends AnyVal {
  def apply(value: Value[U, A])(implicit numeric: Numeric[U]): Value[U, B] = Value(numeric.times(factor, value.value))
}

trait ConvertLowPriority {
  implicit def fromMacro[U, A <: Term, B <: Term]: Convert[U, A, B] = macro Macro.materializeConvert[U, A, B]
}

object Convert extends ConvertLowPriority {
  implicit def default[U, A <: Term](implicit numeric: Numeric[U]): Convert[U, A, A] = new Convert(numeric.one)
}
