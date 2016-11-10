package unu

import org.scalatest.{FlatSpec, Matchers}

import scala.language.experimental.macros
import scala.language.implicitConversions
import unit.length._
import unit.time._
import unit.mass._
import unit.energy._
import unu.number._

import spire.std.any._

class UnuSpec extends FlatSpec with Matchers {

  val epsilon = 1e-8d

  "Converting between units of the same dimensions" should "be correct for sq ft to sq m" in {
    val `sq ft in sq m` = {
      val x: (Double ~ (ft ** ft)) = Value(1d)
      x.in[m ** m]
    }
    `sq ft in sq m`.value shouldEqual (((2.54d * 12d) * (2.54d * 12d)) / (100d * 100d)) +- epsilon
  }

  it should "be correct for sq m to sq ft" in {
    val `sq m in sq ft` = {
      val x: (Double ~ (m ** m)) = Value(1d)
      x.in[ft ** ft]
    }
    `sq m in sq ft`.value shouldEqual ((100d * 100d) / ((2.54d * 12d) * (2.54d * 12d))) +- epsilon
  }

  it should "be correct for cubic m to cubic ft" in {
    val m3: (Double ~ (m ^ `3`)) = Value(1d)

    m3.in[ft ^ `3`].value shouldEqual ((100d * 100d * 100d) / ((2.54d * 12d) * (2.54d * 12d) * (2.54d * 12d))) +- epsilon

    println(s"1 cubic meter is ${m3.in[ft ^ `3`]} cubic feet")
  }

  val time: (Double ~ hr) = Value(2d)
  val distance: (Double ~ mi) = Value(120d)

  val speed = distance / time

  "Converting between speeds" should "be correct for derived unit mph" in {
    val mph = Term.DerivedUnit[mi / hr](1, 1)
    type mph = mph.type

    val mphV = speed.in[mph]
    mphV.value shouldEqual 60d
  }

  it should "be correct for kph" in {
    type kph = km / hr

    val kphV = speed.in[kph]
    kphV.value shouldEqual (60d * 1.609344d) +- epsilon
  }

  it should "be correct for m/s" in {
    val mpsV = speed.in[m / s]
    mpsV.value shouldEqual (60d * 1.609344d * (1000d / (60d * 60d))) +- epsilon
  }

  "Summing values of the same dimensions but different units" should "be correct and output in the desired units" in {
    val d1: (BigDecimal ~ in) = Value(2d)
    val d2: (BigDecimal ~ ft) = Value(2d)
    val d3: (BigDecimal ~ cm) = Value(2d)

    val total: (BigDecimal ~ in) = d1 + d2 + d3

    total.value shouldEqual BigDecimal(2d + (2d * 12d) + (2d / 2.54d)) +- epsilon
    total.in[cm].value shouldEqual BigDecimal((2d * 2.54d) + (2d * (12d * 2.54d)) + 2d) +- epsilon

    val mile: (BigDecimal ~ mi) = Value(1d)

    mile.in[km].value shouldEqual BigDecimal(1.609344d) +- epsilon
  }

  val energy = {
    val speed: (Double ~ (m / s)) = Value(10d)
    val mass: (Double ~ kg) = Value(5d)

    //    (speed * 0.5 * (speed * speed)).in[kg ** ((m / s) ^ Nat._2)]
    (mass * 0.5 * (speed * speed)).in[kg ** ((m / s) ^ `2`)]
  }

  "Multiplying values" should "be correct and output in the desired units" in {
    energy.value shouldEqual (5d * 0.5d * (10d * 10d)) +- epsilon
  }

  "Converting energy" should "be correct for joules" in {
    energy.in[joule].value shouldEqual (5d * 0.5d * (10d * 10d)) +- epsilon
  }

  it should "be correct for calories" in {
    energy.in[cal].value shouldEqual 59.75143403d +- epsilon
  }

  it should "be correct for kcals" in {
    energy.in[kcal].value shouldEqual 0.05975143d +- epsilon
  }

  "Dividing values with the same units" should "be correct and produce a dimensionless result" in {
    val meter: (Double ~ m) = Value(1d)
    val foot: (Double ~ ft) = Value(1d)

    (meter / foot).in[Term.Dimensionless].value shouldEqual (100d / (2.54d * 12d)) +- epsilon
  }

  "Using type aliases" should "work" in {
    trait Blah {
      type T <: Term
    }
    object Blah {
      object ft extends Blah {
        type T = ft
      }
      object m extends Blah {
        type T = m
      }
    }

    val d2: (BigDecimal ~ ft) = Value(2d)
    def f[C <: Blah](c: C) = (d2 * Value[BigDecimal, c.T](3d)).in[m ** c.T]

    f(Blah.ft).in[ft ** ft].value shouldEqual BigDecimal(2d * 3d) +- epsilon
    f(Blah.m).in[ft ** ft].value shouldEqual BigDecimal(2d * 3d * (100d / (2.54d * 12d))) +- epsilon
  }

  "Raising to a power" should "work for naturals" in {
    val speed: (Double ~ (mi / s)) = Value(10d)

    speed.^[Nat.`2`].in[(mi / s) ^ `2`].value shouldEqual 100d +- epsilon

    speed.^[Nat.`2`].in[(km / s) ^ `2`].value shouldEqual 100d * 1.609344d * 1.609344d +- epsilon

    speed.^[Nat.`3`].in[(mi / s) ^ `3`].value shouldEqual 1000d +- epsilon

    speed.^[Nat.`3`].in[(km / s) ^ `3`].value shouldEqual 1000d * 1.609344d * 1.609344d * 1.609344d +- epsilon
  }

  it should "be correct for rationals" in {
    val speed: (Double ~ (mi / s)) = Value(10d)

    speed.^^[`1 / 2`].in[(mi / s) ^ `1 / 2`].value shouldEqual math.sqrt(10d) +- epsilon

    speed.^^[`1 / 2`].in[(km / s) ^ `1 / 2`].value shouldEqual math.sqrt(10d) * math.sqrt(1.609344d) +- epsilon

    speed.^^[`1 / 3`].in[(mi / s) ^ `1 / 3`].value shouldEqual math.pow(10d, 1d / 3d) +- epsilon

    speed.^^[`1 / 3`].in[(km / s) ^ `1 / 3`].value shouldEqual math.pow(10d, 1d / 3d) * math.pow(1.609344d, 1d / 3d) +- epsilon

    speed.^^[`2 / 3`].in[(mi / s) ^ `2 / 3`].value shouldEqual math.pow(10d, 2d / 3d) +- epsilon

    speed.^^[`2 / 3`].in[(km / s) ^ `2 / 3`].value shouldEqual math.pow(10d, 2d / 3d) * math.pow(1.609344d, 2d / 3d) +- epsilon
  }
}