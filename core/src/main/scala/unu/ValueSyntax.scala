package unu

import number._

import spire.algebra._

case class ValueSyntax[U, A <: Term](value: U) extends AnyVal {
  @inline private def v: Value[U, A] = Value(value)

  def map[V](f: U => V): Value[V, A] = Value(f(value))

  def in[B <: Term](implicit msemigroup: MultiplicativeSemigroup[U], convert: Convert[U, A, B]): Value[U, B] = convert(v)

  def unary_-(implicit agroup: AdditiveGroup[U]): Value[U, A] = Value(agroup.negate(value))

  def +[B <: Term, C <: Term](that: Value[U, B])(implicit semiring: Semiring[U], convertA: Convert[U, A, C], convertB: Convert[U, B, C]): Value[U, C] = Value(semiring.plus(convertA(v).value, convertB(that).value))
  def -[B <: Term, C <: Term](that: Value[U, B])(implicit rng: Rng[U], convertA: Convert[U, A, C], convertB: Convert[U, B, C]): Value[U, C] = Value(rng.minus(convertA(v).value, convertB(that).value))

  def *[B <: Term, C <: Term](that: Value[U, B])(implicit msemigroup: MultiplicativeSemigroup[U], convert: Convert[U, A ** B, C]): Value[U, C] = convert(Value(msemigroup.times(value, that.value)))
  def /[B <: Term, C <: Term](that: Value[U, B])(implicit mgroup: MultiplicativeGroup[U], convert: Convert[U, A / B, C]): Value[U, C] = convert(Value(mgroup.div(value, that.value)))

  def ^[N <: Nat](implicit nroot: MultiplicativeSemigroup[U], n: NatValue[N]): Value[U, A ^ N] = Value(nroot.prodn(value, n.value))

  def *(that: U)(implicit msemigroup: MultiplicativeSemigroup[U]): Value[U, A] = Value(msemigroup.times(value, that))
  def /(that: U)(implicit mgroup: MultiplicativeGroup[U]): Value[U, A] = Value(mgroup.div(value, that))
}
