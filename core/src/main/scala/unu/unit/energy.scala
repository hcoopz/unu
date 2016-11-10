package unu
package unit

import number._

object energy {
  type joule = mass.kg * ((length.m / time.s) ^ `2`)

  val cal = Term.DerivedUnit[joule](1000, 4184)
  type cal = cal.type

  val kcal = Term.DerivedUnit[cal](1, 1000)
  type kcal = kcal.type
}