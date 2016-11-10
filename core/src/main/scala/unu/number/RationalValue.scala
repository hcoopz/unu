package unu.number

import scala.language.experimental.macros

case class RationalValue[R](num: Int, denom: Int)

object RationalValue {
  implicit def fromMacro[R <: Rational]: RationalValue[R] = macro unu.Macro.materializeRationalValue[R]
}
