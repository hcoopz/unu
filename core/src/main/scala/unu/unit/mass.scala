package unu
package unit

object mass {
  val kg = Term.BaseUnit()
  type kg = kg.type

  val g = Term.DerivedUnit[kg](1000, 1)
  type g = g.type

  val mg = Term.DerivedUnit[g](1000, 1)
  type mg = mg.type
}