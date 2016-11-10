package unu.number

import scala.language.experimental.macros

case class NatValue[N](value: Int) extends AnyVal

object NatValue {
  implicit def fromMacro[N <: Nat]: NatValue[N] = macro unu.Macro.materializeNatValue[N]
}