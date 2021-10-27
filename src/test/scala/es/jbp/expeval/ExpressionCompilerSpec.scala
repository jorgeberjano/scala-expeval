package es.jbp.expeval

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable
import scala.util.{Try, Failure, Success}

class ExpressionCompilerSpec extends AnyFlatSpec {

  val variableMap = mutable.Map[String, Value]()

  variableMap += ("x" -> Value(10))
  variableMap += ("y" -> Value("2"))

  val myVariableResolver: VariableResolver = {
    case name: String if (variableMap.contains(name)) => variableMap(name)
    case _ => Value(0)
  }

  val functionMap = mutable.Map[String, Function]()
  functionMap += ("addition" -> AdditionFunction())

  val myFunctionResolver: FunctionResolver = {
    case name: String if (functionMap.contains(name)) => functionMap(name)
    case _ => InexistentFunction()
  }

  given VariableResolver = myVariableResolver;

  given FunctionResolver = myFunctionResolver;


  def evaluate(expression: String): Try[Value] = {
    val compiler = ExpresionCompiler()

    val result = compiler.compile(expression)
    result match {
      case Success(node) => {
        val resultValue = node.evaluate()
        Success(resultValue)
      }
      case Failure(exception) => Failure(exception)
    }
  }

  "3 * -1" should "evaluate to -3" in {
    val tryNode = evaluate("3 * -1")

    tryNode.get.asBigDecimal() shouldBe BigDecimal("-3")
  }

  "9 / - (-1 * -3)" should "evaluate to -3" in {
    val tryNode = evaluate("9 / - (-1 * -3)")

    tryNode.get.asBigDecimal() shouldBe BigDecimal("-3")
  }


  "1 + 2 * 3" should "evaluate to 7" in {
    val tryNode = evaluate("1 + 2 * 3")

    tryNode.get.asBigDecimal() shouldBe BigDecimal("7")
  }

  "x * 2 " should "evaluate to 20" in {
    val tryNode = evaluate("x * 2")

    tryNode.get.asBigDecimal() shouldBe BigDecimal("20")
  }

  "11 * y" should "evaluate to 22" in {
    val tryNode = evaluate("11 * y")

    tryNode.get.asBigDecimal() shouldBe BigDecimal("22")
  }

  "10 - (4 / 2)" should "evaluate to 8" in {
    val tryNode = evaluate("10 - (4 / 2)")

    tryNode.get.asBigDecimal() shouldBe BigDecimal("8")
  }

  "(10 - 4) / 2" should "evaluate to 3" in {
    val tryNode = evaluate("(10 - 4) / 2")

    tryNode.get.asBigDecimal() shouldBe BigDecimal("3")
  }

  "(7 % (5 - 1) - 1)" should "evaluate to 2" in {
    val tryNode = evaluate("(7 % (5 - 1) - 1)")

    tryNode.get.asBigDecimal() shouldBe BigDecimal("2")
  }

  "((6 - 3) * (10 - 4)) / (7 % (5 - 1) - 1)" should "evaluate to 9" in {
    val tryNode = evaluate("((6 - 3) * (10 - 4)) / (7 % (5 - 1) - 1)")

    tryNode.get.asBigDecimal() shouldBe BigDecimal("9")
  }
}
