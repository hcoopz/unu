package unu.number

sealed trait Nat extends Product with Serializable
object Nat {
  case object One extends Nat
  type One = One.type

  case class Succ[N <: Nat] private () extends Nat

  type `1` = Nat.One
  type `2` = Succ[`1`]
  type `3` = Succ[`2`]
  type `4` = Succ[`3`]
}
