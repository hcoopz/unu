package unu.number

sealed trait Nat extends Product with Serializable
object Nat {
  case object One extends Nat
  case class Succ[N <: Nat] private () extends Nat
}
