package unu

sealed trait Nat extends Product with Serializable
object Nat {
  case object One extends Nat
  case class Succ[N <: Nat] private () extends Nat

  type _1 = One.type
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
}
