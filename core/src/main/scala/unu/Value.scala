package unu

case class Value[U, A <: Term](value: U) extends AnyVal {
  override def toString = value.toString
}
