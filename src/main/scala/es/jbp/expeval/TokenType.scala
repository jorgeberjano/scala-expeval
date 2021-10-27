package es.jbp.expeval

import scala.util.matching.Regex

sealed class TokenType(pattern: String) {
  val regex = Regex(pattern)

  def matches(token: String) = regex.matches(token)

}

case object TokenSpace extends TokenType("[\\s\\t\\r\\n]+")

case object TokenOperator extends TokenType("(<|<=|>|>=|==|!=|\\+|\\-|\\*|/|\\^|%|[Aa][Nn][Dd]|[Oo][Rr])")

case object TokenNumber extends TokenType("[0-9]*\\.?[0-9]*([eE][-+]?[0-9]*)?")

case object TokenColon extends TokenType(",")

case object TokenIdentifier extends TokenType("[A-Za-z_][A-Za-z_0-9]*[\\.]*[A-Za-z_0-9]*")

case object TokenString extends TokenType("\"(\\.|[^\"])*\"")

case object TokenOpenParenthesis extends TokenType("\\(")

case object TokenCloseParenthesis extends TokenType("\\)")

type TokenSeq = Seq[TokenType]

