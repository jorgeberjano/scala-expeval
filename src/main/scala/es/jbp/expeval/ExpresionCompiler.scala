package es.jbp.expeval

import scala.util.{Failure, Success, Try}
import scala.collection.mutable
import es.jbp.expeval.TokenSeq
import es.jbp.expeval.TokenType

class CompilerException(s: String) extends Exception(s)

class ExpresionCompiler {

  val MISSING_CLOSING_PARENTHESIS = "Falta un paréntesis de cierre"
  val INVALID_OPERATOR = "Operador inválido"
  val SYNTAX_ERROR = "Error de sintaxis"
  val VARIABLE_NOT_FOUND = "Variable no encontrada"
  val FUNCTION_NOT_FOUND = "Función no encotrada"
  val TOO_MANY_CLOSING_PARENTHESIS = "Demasiados paréntesis de cierre en la función"
  val TOO_MANY_PARAMETERS = "Número de parámetros excesivo"
  val MISSING_RIGHT_OPERAND = "Falta el operando derecho"
  val MISSING_LEFT_OPERAND = "Falta el operando izquierdo"
  val MISSING_OPERANDS = "Faltan los operandos"
  val UNRECOGNIZED_TOKEN = "Token no reconocido"
  val MISSING_EXPRESSION = "Falta la expresión"
  val UNEXPECTED_CLOSING_PARENTHESIS = "No se esperaba el paréntesis de cierre"
  val MISSING_FUNCTION_CLOSING_PARENTHESIS = "Falta el paréntesis de cierre en la función"
  val TOO_FEW_PARAMETERS = "Número de parámetros insuficiente"

  type TokenList = mutable.ArrayBuffer[Token];

  def compile(expression: String): Try[ExpressionNode] = Try {

    val tokenSeq = lexicalAnalysis(expression).filter(t => t.tokenType != TokenSpace)
    if (tokenSeq.isEmpty) {
      error(MISSING_EXPRESSION, "", 0)
    }
    syntacticalAnalysis(tokenSeq)
  }

  private def error(message: String, tokens: String, position: Int) =
    throw new CompilerException(message + " (" + position + "): " + tokens);

  private def mid(texto: String, position: Int, n: Int): String = texto.substring(position, position + n)


  def lexicalAnalysis(expression: String): TokenList = {
    val tokenList = mutable.ArrayBuffer[Token]()

    if (expression.isEmpty) {
      error(MISSING_EXPRESSION, "", 0)
    }

    var baseIndex = 0
    var currentIndex = 0
    var lastToken: Option[Token] = None

    while (currentIndex < expression.length) {
      val fragment = mid(expression, baseIndex, currentIndex - baseIndex + 1)

      determinarTipoDeToken(fragment) match {
        case None => {
          if (lastToken != None) { // Se agrega el último token valido
            tokenList += lastToken.get
            lastToken = None
            baseIndex = currentIndex
          } else {
            currentIndex += 1
          }
        }
        case Some(tokenType: TokenType) => {
          lastToken = Some(Token(tokenType = tokenType, text = fragment, position = baseIndex))
          currentIndex += 1
        }
      }
    }

    if (lastToken != None) {
      tokenList += lastToken.get
    }
    tokenList
  }

  def determinarTipoDeToken(token: String): Option[TokenType] = {

    val tokenTypeSeq: Seq[TokenType] = List(
      TokenSpace,
      TokenOperator,
      TokenNumber,
      TokenColon,
      TokenIdentifier,
      TokenString,
      TokenOpenParenthesis,
      TokenCloseParenthesis
    )

    for (tokenType <- tokenTypeSeq) {
      if (tokenType.matches(token)) {
        return Some(tokenType)
      }
    }
    None
  }

  /**
   * Realiza el análisis sintáctico
   */
  def syntacticalAnalysis(tokenList: TokenList): ExpressionNode = {
    val node = parse(tokenList, 0, tokenList.length - 1)
    node match {
      case Some(n) => n
      case None => error(MISSING_EXPRESSION, "", 0)
    }
  }

  /**
   * Parsea el listado de tokens
   */
  private def parse(tokenList: TokenList, firstIndex: Int, lastIndex: Int): Option[ExpressionNode] = {
    if (tokenList.isEmpty || firstIndex >= tokenList.size || lastIndex > tokenList.size || firstIndex > lastIndex) {
      return None
    }

    val (operatorToken, operatorIndex) = findLessPriorityOperand(tokenList, firstIndex, lastIndex)

    operatorToken match {
      // Hay un operador: se parsean los operandos y se agregan
      case Some(o) => createOperatorNode(o, operatorIndex, tokenList, firstIndex, lastIndex)
      // No hay operadores...
      case None => createNonOperatorNode(tokenList, firstIndex, lastIndex)
    }
  }

  private def createOperator(operatorToken: Token): (Value, Value) => Value = {
    operatorToken.text match {
      case "+" => (a, b) => a + b
      case "-" => (a, b) => a - b
      case "*" => (a, b) => a * b
      case "/" => (a, b) => a / b
      case "%" => (a, b) => a % b
      case _ => error(INVALID_OPERATOR, operatorToken.text, operatorToken.position)
    }
  }

  private def createOperatorNode(operatorToken: Token, operatorIndex: Int, tokenList: TokenList, firstIndex: Int, lastIndex: Int): Option[ExpressionNode] = {
    val subexpressionNode1 = parse(tokenList, firstIndex, operatorIndex - 1)
    val subexpressionNode2 = parse(tokenList, operatorIndex + 1, lastIndex)
    (subexpressionNode1, subexpressionNode2) match {
      case (Some(se1), Some(se2)) => Some(NodeOperation(createOperator(operatorToken), se1, se2))
      case (None, Some(se2)) => createUnaryOperatorNode(operatorToken, Some(se2))
      case (Some(_), None) => error(MISSING_RIGHT_OPERAND, operatorToken.text, operatorToken.position)
      case (None, None) => error(MISSING_OPERANDS, operatorToken.text, operatorToken.position)
    }
  }

  private def createUnaryOperatorNode(operatorToken: Token, subexpressionNode: Option[ExpressionNode]): Option[ExpressionNode] = {
    if (subexpressionNode.isEmpty) {
      error(MISSING_RIGHT_OPERAND, operatorToken.text, operatorToken.position)
    }

    operatorToken.text match {
      case "+" | "-" => Some(NodeOperation(createOperator(operatorToken), NodeValue(Value(BigDecimal(0))), subexpressionNode.get))
      case _ => error(MISSING_LEFT_OPERAND, operatorToken.text, operatorToken.position)
    }
  }

  private def createNonOperatorNode(tokenList: TokenList, firstIndex: Int, lastIndex: Int): Option[ExpressionNode] = {

    if (firstIndex == lastIndex) {
      parseSingleTokenNode(tokenList(firstIndex))
    } else {
      parseMultipleTokenNode(tokenList, firstIndex, lastIndex)
    }
  }

  private def parseSingleTokenNode(token: Token): Option[ExpressionNode] = {
    // Es un literal (numero o cadena), una variable o un atributo
    token.tokenType match {
      case TokenNumber => {
        val decimal = BigDecimal(token.text);
        Some(NodeValue(Value(decimal)))
      }
      case TokenString => Some(NodeValue(new Value(mid(token.text, 1, token.text.length - 2))))
      case TokenIdentifier => Some(NodeVariable(token.text))
      case _ => None
    }
  }


  private def parseMultipleTokenNode(tokenList: TokenList, firstIndex: Int, lastIndex: Int): Option[ExpressionNode] = {

//    if (lastIndex - firstIndex < 2) {
//      error(SYNTAX_ERROR, "", firstIndex);
//    }

    val firstToken = tokenList(firstIndex)
    val secondToken = tokenList(firstIndex + 1)
    val lastToken = tokenList(lastIndex)

    (firstToken.tokenType, secondToken.tokenType, lastToken.tokenType) match {
      case (TokenIdentifier, TokenOpenParenthesis, TokenCloseParenthesis) =>
        Some(NodeFunction(firstToken.text, parseFunctionParameters(tokenList, firstIndex + 1, lastIndex)))
      case (TokenIdentifier, TokenOpenParenthesis, _) =>
        error(MISSING_CLOSING_PARENTHESIS, "", lastToken.position);
      case (TokenOpenParenthesis, _, TokenCloseParenthesis) =>
        parse(tokenList, firstIndex + 1, lastIndex - 1)
      case (TokenOpenParenthesis, _, _) =>
        error(MISSING_CLOSING_PARENTHESIS, "", lastToken.position);
      case (TokenOperator, _, _) => createUnaryOperatorNode(firstToken, parse(tokenList, firstIndex + 1, lastIndex))
      case (_, _, _) => error(SYNTAX_ERROR, "", firstIndex);
    }
  }

//  private def createUnaryOperatorLiteralNode(operatorToken: Token, subjectToken: Token): Option[ExpressionNode] = {
//
//    val subjectNode = parseSingleTokenNode(subjectToken)
//    subjectNode match {
//      case Some(node) => createUnaryOperatorNode(operatorToken, node)
//      case None => error(SYNTAX_ERROR, operatorToken.text, operatorToken.position);
//    }
//
//  }

  private def parseFunctionParameters(tokenList: TokenList, firstIndex: Int, lastIndex: Int): Seq[ExpressionNode] = {
    val parameterNodes = mutable.ArrayBuffer[ExpressionNode]()
    var parenthesisLevel = 0
    var baseIndex = firstIndex
    for (currentIndex <- firstIndex until lastIndex) {
      val token = tokenList(currentIndex)
      token.tokenType match {
        case TokenOpenParenthesis => parenthesisLevel += 1
        case TokenCloseParenthesis => parenthesisLevel -= 1
        case TokenColon if parenthesisLevel == 0 => {
          parameterNodes += parseParameterNode(tokenList, baseIndex, currentIndex)
          baseIndex = currentIndex + 1
        }
      }
      if (parenthesisLevel < 0) error(TOO_MANY_CLOSING_PARENTHESIS, "", token.position)

      val isLastToken = currentIndex == lastIndex - 1
      if (isLastToken && parenthesisLevel == 0) {
        parameterNodes += parseParameterNode(tokenList, baseIndex, currentIndex + 1)
        baseIndex = currentIndex + 1
      }
    }
    parameterNodes.toSeq
  }

  private def parseParameterNode(tokenList: TokenList, firstIndex: Int, lastIndex: Int): ExpressionNode = {
    val parameter = parse(tokenList, firstIndex, lastIndex)
    parameter match {
      case Some(node) => node
      case None => error(MISSING_EXPRESSION, "", lastIndex);
    }
  }


  def findLessPriorityOperand(tokenList: TokenList, firstIndex: Int, lastIndex: Int): (Option[Token], Int) = {
    var candidateToken: Option[Token] = None
    var candidateIndex = -1
    var parenthesisLevel = 0
    // Se busca el operador de menor prioridad de derecha a izquierda
    for (i <- lastIndex to firstIndex by -1) {
      var skipToken = false;
      val token = tokenList(i)
      val currentToken: Option[Token] = token.tokenType match {
        case TokenOpenParenthesis => {
          parenthesisLevel -= 1
          None
        }
        case TokenCloseParenthesis => {
          parenthesisLevel += 1
          None
        }
        // Si hay algun operador que no esté entre parentesis y que no tenga otro operador a su izquierda es candidato
        case TokenOperator if parenthesisLevel == 0 &&
          (i == 0 || i - 1 >= firstIndex && tokenList(i - 1).tokenType != TokenOperator) =>
          Some(token)
        case _ => None
      }
      (candidateToken, currentToken) match {
        case (Some(candidate), Some(current)) if current.priority < candidate.priority => {
          candidateToken = Some(current)
          candidateIndex = i
        }
        case (None, Some(current)) => {
          candidateToken = Some(current)
          candidateIndex = i
        }
        case _ =>
      }
    }
    val lastToken = tokenList(lastIndex)
    if (parenthesisLevel > 0) {
      error(UNEXPECTED_CLOSING_PARENTHESIS, "", lastToken.position)
    }
    else if (parenthesisLevel < 0) {
      error(MISSING_CLOSING_PARENTHESIS, "", lastToken.position)
    }
    (candidateToken, candidateIndex)
  }

}



