package unu.number

sealed trait Rational extends Product with Serializable {
  type Num <: Nat
  type Denom <: Nat
}

object Rational {
  type Fraction[N <: Nat, D <: Nat] = Rational { type Num = N; type Denom = D }
  type FromNat[N <: Nat] = Fraction[N, Nat.One]
}