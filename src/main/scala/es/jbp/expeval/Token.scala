package es.jbp.expeval

class Token(val tokenType: TokenType, val text: String, val position: Int) {

  val priority = text match {
    case "OR" => 0
    case "AND" => 1
    case "+" | "-" => 2
    case "*" | "/" | "%" => 4
    case _ => 5
  }

}
