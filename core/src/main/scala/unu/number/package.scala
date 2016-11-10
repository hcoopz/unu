package unu

package object number {
  type `1` = Rational.FromNat[Nat.`1`]
  type `2` = Rational.FromNat[Nat.`2`]
  type `3` = Rational.FromNat[Nat.`3`]
  type `4` = Rational.FromNat[Nat.`4`]

  type `1 / 2` = Rational.Fraction[Nat.`1`, Nat.`2`]
  type `1 / 3` = Rational.Fraction[Nat.`1`, Nat.`3`]
  type `2 / 3` = Rational.Fraction[Nat.`2`, Nat.`3`]
}
