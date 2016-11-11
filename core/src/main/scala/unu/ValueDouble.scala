package unu

import spire.std.any._
import spire.syntax.all._
import unu.number._

import scala.language.experimental.macros
import scala.language.higherKinds

case class ConvertDouble[A <: Term, B <: Term](f: Double) extends AnyVal {
  @inline def apply(u: Double): Double = u * f
}

trait ConvertDoubleLowPriority {
  implicit def fromMacro[A <: Term, B <: Term]: ConvertDouble[A, B] = macro Macro.materializeConvertDouble[A, B]
}

object ConvertDouble extends ConvertDoubleLowPriority {
  implicit def default[A <: Term]: ConvertDouble[A, A] = new ConvertDouble(1d)
}

final case class ValueDouble[A <: Term](value: Double) extends AnyVal {
  override def toString = value.toString

  def transform(f: Double => Double): ValueDouble[A] = ValueDouble(f(value))

  def in[B <: Term](implicit convert: ConvertDouble[A, B]): ValueDouble[B] = ValueDouble(convert(value))

  def `unary_-`: ValueDouble[A] = ValueDouble(-value)

  def +[B <: Term](that: ValueDouble[B])(implicit convert: ConvertDouble[B, A]): ValueDouble[A] = ValueDouble(value + convert(that.value))
  def +~[B <: Term, C <: Term](that: ValueDouble[B])(implicit convertA: ConvertDouble[A, C], convertB: ConvertDouble[B, C]): ValueDouble[C] = ValueDouble(convertA(value) + convertB(that.value))
  def -[B <: Term](that: ValueDouble[B])(implicit convert: ConvertDouble[B, A]): ValueDouble[A] = ValueDouble(value - convert(that.value))
  def -~[B <: Term, C <: Term](that: ValueDouble[B])(implicit convertA: ConvertDouble[A, C], convertB: ConvertDouble[B, C]): ValueDouble[C] = ValueDouble(convertA(value) - convertB(that.value))

  def *[B <: Term](that: ValueDouble[B]): ValueDouble[A * B] = ValueDouble(value * that.value)
  def /[B <: Term](that: ValueDouble[B]): ValueDouble[A / B] = ValueDouble(value / that.value)

  def ^[N <: Nat](implicit n: NatValue[N]): ValueDouble[A ^ unu.number.Rational.FromNat[N]] = ValueDouble(value.pow(n.value))

  def ^^[R <: Rational](implicit r: RationalValue[R]): ValueDouble[A ^ R] = ValueDouble(value.fpow(r.num.toDouble / r.denom.toDouble))

  def `*_scalar`(that: Double): ValueDouble[A] = ValueDouble(value * that)
  def `/_scalar`(that: Double): ValueDouble[A] = ValueDouble(value / that)
}
