package es.jbp.expeval

import java.math.BigDecimal
import es.jbp.expeval.Value
import es.jbp.expeval.Function

sealed abstract class ExpressionNode

case class NodeOperation(operation: (Value, Value) => Value, op1: ExpressionNode, op2: ExpressionNode) extends ExpressionNode

case class NodeValue(value: Value) extends ExpressionNode

case class NodeVariable(name: String) extends ExpressionNode

case class NodeFunction(name: String, nodeParams: Seq[ExpressionNode]) extends ExpressionNode

extension (node: ExpressionNode)(using variableResolver: VariableResolver, functionResolver: FunctionResolver)
  def evaluate(): Value = node match {
      case NodeOperation(operation, op1, op2) => operation(op1.evaluate(), op2.evaluate())
      case NodeValue(value) =>  value
      case NodeVariable(name) => variableResolver(name)
      case NodeFunction(name, nodeParams) => functionResolver(name).evaluate(nodeParams.map(n => n.evaluate()))
      case _ => Value(BigDecimal(0))
    }

type VariableResolver = String => Value

type FunctionResolver = String => Function

