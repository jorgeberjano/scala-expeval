package es.jbp.expeval

class Value(val value: Any) {

  def +(other: Value) = Value(this.asBigDecimal() + other.asBigDecimal())

  def -(other: Value) = Value(this.asBigDecimal() - other.asBigDecimal())

  def *(other: Value) = Value(this.asBigDecimal() * other.asBigDecimal())

  def /(other: Value) = Value(this.asBigDecimal() / other.asBigDecimal())

  def %(other: Value) = Value(this.asBigDecimal() % other.asBigDecimal())

  def asBigDecimal(): BigDecimal = value match {
    case bd: scala.math.BigDecimal => bd
    case s: String => BigDecimal(s)
    case i: Int => BigDecimal(i)
    case d: Double => BigDecimal(d)
    case x: _ => BigDecimal(x.toString())
  }
}
