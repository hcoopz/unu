package unu

import number._
import spire.algebra._

final case class Value[U, A <: Term](value: U) extends AnyVal {
  override def toString = value.toString

  @inline private def v: Value[U, A] = Value(value)

  def map[V](f: U => V): Value[V, A] = Value(f(value))

  def transform(f: U => U): Value[U, A] = Value(f(value))

  def in[B <: Term](implicit msemigroup: MultiplicativeSemigroup[U], convert: Convert[U, A, B]): Value[U, B] = convert(v)

  def unary_-(implicit agroup: AdditiveGroup[U]): Value[U, A] = Value(agroup.negate(value))

  def +[B <: Term](that: Value[U, B])(implicit semiring: Semiring[U], convert: Convert[U, B, A]): Value[U, A] = Value(semiring.plus(value, convert(that).value))
  def +~[B <: Term, C <: Term](that: Value[U, B])(implicit semiring: Semiring[U], convertA: Convert[U, A, C], convertB: Convert[U, B, C]): Value[U, C] = Value(semiring.plus(convertA(v).value, convertB(that).value))
  def -[B <: Term](that: Value[U, B])(implicit rng: Rng[U], convert: Convert[U, B, A]): Value[U, A] = Value(rng.minus(value, convert(that).value))
  def -~[B <: Term, C <: Term](that: Value[U, B])(implicit rng: Rng[U], convertA: Convert[U, A, C], convertB: Convert[U, B, C]): Value[U, C] = Value(rng.minus(convertA(v).value, convertB(that).value))

  def *[B <: Term](that: Value[U, B])(implicit msemigroup: MultiplicativeSemigroup[U]): Value[U, A * B] = Value(msemigroup.times(value, that.value))

  def /[B <: Term](that: Value[U, B])(implicit mgroup: MultiplicativeGroup[U]): Value[U, A / B] = Value(mgroup.div(value, that.value))

  def ^[N <: Nat](implicit nroot: MultiplicativeSemigroup[U], n: NatValue[N]): Value[U, A ^ Rational.FromNat[N]] = Value(nroot.pow(value, n.value))

  def ^^[R <: Rational](implicit nroot: NRoot[U], fractional: spire.math.Fractional[U], r: RationalValue[R]): Value[U, A ^ R] = Value(nroot.fpow(value, fractional.div(fractional.fromLong(r.num), fractional.fromLong(r.denom))))

  def `*_scalar`(that: U)(implicit msemigroup: MultiplicativeSemigroup[U]): Value[U, A] = Value(msemigroup.times(value, that))
  def `/_scalar`(that: U)(implicit mgroup: MultiplicativeGroup[U]): Value[U, A] = Value(mgroup.div(value, that))
}
