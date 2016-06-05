package unu
package unit

object time {
  val s = Term.BaseUnit()
  type s = s.type

  val min = Term.DerivedUnit[s](1, 60)
  type min = min.type

  val hr = Term.DerivedUnit[min](1, 60)
  type hr = hr.type
}