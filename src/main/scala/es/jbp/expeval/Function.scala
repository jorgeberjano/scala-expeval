package es.jbp.expeval

trait Function {
  def evaluate(parameters: Seq[Value]): Value
  def getParameterCount(): Int
  def allowOmitParameters(): Boolean
}

class InexistentFunction extends Function {
  def allowOmitParameters(): Boolean = ???
  def evaluate(parameters: Seq[Value]): Value = ???
  def getParameterCount(): Int = ???
}

class AdditionFunction extends Function {

  def evaluate(parameters: Seq[Value]): Value = Value(parameters.map(v => v.asBigDecimal()).sum)
  def getParameterCount(): Int = 2
  def allowOmitParameters(): Boolean = false
}