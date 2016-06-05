package unu
package unit

object length {
  val m = Term.BaseUnit()
  type m = m.type

  val km = Term.DerivedUnit[m](1, 1000)
  type km = km.type

  val cm = Term.DerivedUnit[m](100, 1)
  type cm = cm.type

  val in = Term.DerivedUnit[cm](100, 254)
  type in = in.type

  val ft = Term.DerivedUnit[in](1, 12)
  type ft = ft.type

  val yd = Term.DerivedUnit[ft](1, 3)
  type yd = yd.type

  val mi = Term.DerivedUnit[yd](1, 1760)
  type mi = mi.type
}