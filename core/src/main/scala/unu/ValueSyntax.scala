package unu

case class ValueSyntax[U, A <: Term](value: U) extends AnyVal {
  @inline private def v: Value[U, A] = Value(value)

  def map[V](f: U => V): Value[V, A] = Value(f(value))

  def in[B <: Term](implicit numeric: Numeric[U], convert: Convert[U, A, B]): Value[U, B] = convert(v)

  def unary_-(implicit numeric: Numeric[U]): Value[U, A] = Value(numeric.negate(value))

  def +[B <: Term, C <: Term](that: Value[U, B])(implicit numeric: Numeric[U], convertA: Convert[U, A, C], convertB: Convert[U, B, C]): Value[U, C] = Value(numeric.plus(convertA(v).value, convertB(that).value))
  def -[B <: Term, C <: Term](that: Value[U, B])(implicit numeric: Numeric[U], convertA: Convert[U, A, C], convertB: Convert[U, B, C]): Value[U, C] = Value(numeric.minus(convertA(v).value, convertB(that).value))

  def *[B <: Term, C <: Term](that: Value[U, B])(implicit numeric: Numeric[U], convert: Convert[U, A ** B, C]): Value[U, C] = convert(Value(numeric.times(value, that.value)))
  def /[B <: Term, C <: Term](that: Value[U, B])(implicit fractional: Fractional[U], convert: Convert[U, A / B, C]): Value[U, C] = convert(Value(fractional.div(value, that.value)))

  def *(that: U)(implicit numeric: Numeric[U]): Value[U, A] = Value(numeric.times(value, that))
  def /(that: U)(implicit fractional: Fractional[U]): Value[U, A] = Value(fractional.div(value, that))
}
