package unu.benchmark

import org.openjdk.jmh.annotations.Benchmark
import spire.std.any._
import unu._
import unu.number._

object FeetToInches {
  def unchecked(a: Double): Double = {
    a * 2.54d / 100d
  }

  def unuGeneric(a: Double): Double = {
    Value[Double, unit.length.in](a).in[unit.length.m].value
  }

  def unuSpecialized(a: Double): Double = {
    ValueDouble[unit.length.in](a).in[unit.length.m].value
  }
}

object Energy {
  def unchecked(distanceMi: Double, timeHr: Double, massKg: Double): Double = {
    0.5d * massKg * math.pow((distanceMi * 1.609344d * 1000d) / (timeHr * 60d * 60d), 2)
  }

  def unuGeneric(distance: Double ~ unu.unit.length.mi, time: Double ~ unu.unit.time.hr, mass: Double ~ unu.unit.mass.kg): Double ~ unu.unit.energy.joule = {
    (mass * (distance / time).^[Nat.`2`] `*_scalar` 0.5d).in[unu.unit.energy.joule]
  }

  def unuSpecialized(distance: ValueDouble[unu.unit.length.mi], time: ValueDouble[unu.unit.time.hr], mass: ValueDouble[unu.unit.mass.kg]): ValueDouble[unu.unit.energy.joule] = {
    (mass * (distance / time).^[Nat.`2`] `*_scalar` 0.5d).in[unu.unit.energy.joule]
  }
}

object Adding {
  def unchecked(a: Double): Double = {
    a + a / unit.length.ft.denom
  }

  def unuGeneric(a: Double): Double = {
    (Value[Double, unit.length.ft](a) + Value[Double, unit.length.in](a)).value
  }

  def unuSpecialized(a: Double): Double = {
    (ValueDouble[unit.length.ft](a) + ValueDouble[unit.length.in](a)).value
  }
}

class Benchmarks {
  @Benchmark
  def feetToInchesUnchecked: Unit = FeetToInches.unchecked(3)

  @Benchmark
  def feetToInchesUnuGeneric: Unit = FeetToInches.unuGeneric(3)

  @Benchmark
  def feetToInchesUnuSpecialized: Unit = FeetToInches.unuSpecialized(3)

  @Benchmark
  def energyUnchecked: Unit = Energy.unchecked(3, 4, 5)

  @Benchmark
  def energyUnuGeneric: Unit = Energy.unuGeneric(Value(3), Value(4), Value(5))

  @Benchmark
  def energyUnuSpecialized: Unit = Energy.unuSpecialized(ValueDouble(3), ValueDouble(4), ValueDouble(5))

  @Benchmark
  def addingUnchecked: Unit = Adding.unchecked(3)

  @Benchmark
  def addingUnuGeneric: Unit = Adding.unuGeneric(3)

  @Benchmark
  def addingUnuSpecialized: Unit = Adding.unuSpecialized(3)
}