package unu

import scala.language.experimental.macros
import scala.language.implicitConversions
import unit.length._
import unit.time._
import unit.mass._
import unit.energy._
import unu.number._

object Unu extends App {

  val mph = Term.DerivedUnit[mi / hr](1, 1)
  type mph = mph.type

  type kph = km / hr

  val `sq m in sq ft` = {
    val x: (Double ~ (ft ** ft)) = Value(1d)
    x.in[m ** m]
  }
  println(s"There are ${`sq m in sq ft`} sq m in a sq ft")

  val `sq ft in sq m` = {
    val x: (Double ~ (m ** m)) = Value(1d)
    x.in[ft ** ft]
  }
  println(s"There are ${`sq ft in sq m`} sq ft in a sq m")

  val time: (Double ~ hr) = Value(2d)
  val distance: (Double ~ mi) = Value(120d)

  val speed = distance / time

  val mphV = speed.in[mph]
  val kphV = speed.in[kph]
  val mpsV = speed.in[m / s]

  println(s"120 miles in 2 hours is $mphV mph, $kphV kph, and $mpsV m/s")

  val d1: (BigDecimal ~ in) = Value(2d)
  val d2: (BigDecimal ~ ft) = Value(2d)
  val d3: (BigDecimal ~ cm) = Value(2d)

  val total: (BigDecimal ~ in) = d1 + d2 + d3

  println(s"2 in + 2 ft + 2 cm = $total in = ${total.in[cm]} cm")

  val mile: (BigDecimal ~ mi) = Value(1d)

  println(s"a mile is ${mile.in[km]} km")

  val energy = {
    val speed: (Double ~ (m / s)) = Value(10d)
    val mass: (Double ~ kg) = Value(5d)

//    (speed * 0.5 * (speed * speed)).in[kg ** ((m / s) ^ Nat._2)]
    (mass * 0.5 * (speed * speed)).in[kg ** ((m / s) ^ `2`)]
  }
  println(s"a 5kg object moving at 10 m/s has an energy of $energy kg (m/s)^2 = ${energy.in[joule]} J = ${energy.in[cal]} cal = ${energy.in[kcal]} kcal")

  val m3: (Double ~ (m ^ `3`)) = Value(1d)

  println(s"1 cubic meter is ${m3.in[ft ^ `3`]} cubic feet")

  val meter: (Double ~ m) = Value(1d)
  val foot: (Double ~ ft) = Value(1d)

  println(s"the ratio of 1 m to 1 ft is ${(meter / foot).in[Term.Dimensionless]}")

  trait Blah {
    type T <: Term
  }
  object Blah {
    object ft extends Blah { type T = ft }
    object m extends Blah { type T = m }
  }

  def f[C <: Blah](c: C) = (d2 * Value[BigDecimal, c.T](3d)).in[m ** c.T]

  println(s"${f(Blah.ft).in[ft ** ft]}")
  println(s"${f(Blah.m).in[ft ** ft]}")
}