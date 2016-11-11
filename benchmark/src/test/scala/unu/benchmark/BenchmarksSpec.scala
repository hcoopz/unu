package unu.benchmark

import unu._
import org.scalatest.{FlatSpec, Matchers}

class BenchmarksSpec extends FlatSpec with Matchers {
  val epsilon = 1e-8d

  "Adding benchmarks" should "produce the same result" in {
    val arg = 3d
    Adding.unchecked(arg) shouldEqual Adding.unuGeneric(arg) +- epsilon
    Adding.unchecked(arg) shouldEqual Adding.unuSpecialized(arg) +- epsilon
  }

  "Converting benchmarks" should "produce the same result" in {
    val arg = 3d
    FeetToInches.unchecked(arg) shouldEqual FeetToInches.unuGeneric(arg) +- epsilon
    FeetToInches.unchecked(arg) shouldEqual FeetToInches.unuSpecialized(arg) +- epsilon
  }

  "Energy benchmarks" should "produce the same result" in {
    val arg1 = 3d
    val arg2 = 4d
    val arg3 = 5d
    Energy.unchecked(arg1, arg2, arg3) shouldEqual Energy.unuGeneric(Value(arg1), Value(arg2), Value(arg3)).value +- epsilon
    Energy.unchecked(arg1, arg2, arg3) shouldEqual Energy.unuSpecialized(ValueDouble(arg1), ValueDouble(arg2), ValueDouble(arg3)).value +- epsilon
  }
}
