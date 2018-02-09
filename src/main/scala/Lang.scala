package com.todesking.prety

object Lang {
  def parse(s: String): Seq[(String, AST)] =
    Parser.parseAll(Parser.all, s) match {
      case Parser.NoSuccess(msg, _) =>
        throw new RuntimeException(s"Parse error($msg): $s")
      case Parser.Success(asts, _) =>
        asts
    }

  object Parser extends scala.util.parsing.combinator.RegexParsers {
    def all: Parser[Seq[(String, AST)]] = repsep(pred, ",")

    def pred = (ident <~ ':') ~ expr ^^ { case id ~ expr => (id.name -> expr) }
    def expr = expr1 ~ (op ~ expr1).? ^^ {
      case lhs ~ Some(op ~ rhs) => AST.Op(lhs, op, rhs)
      case lhs ~ None => lhs
    }
    def expr1 = measure | lit
    def measure = (the_value | ident) ~ rep('.' ~> name) ^^ {
      case id ~ names =>
        names.foldLeft[AST](id) { (m, id) => AST.Select(m, id) }
    }
    def ident = name ^^ { id => AST.Ident(id) }
    def the_value = "_" ^^ { _ => AST.TheValue }
    def name = "[a-zA-Z_][a-zA-Z_0-9]*".r
    def op = "[-+<>:*/]+".r
    def lit = int
    def int = "[0-9]|[1-9][0-9]*".r ^^ { v => AST.LitInt(v.toInt) }
  }

  sealed abstract class AST(override val toString: String) {
    def names: Set[String]
  }
  object AST {
    case object TheValue extends AST("_") {
      override def names = Set()
    }
    case class Ident(name: String) extends AST(name) {
      override def names = Set(name)
    }
    case class Select(value: AST, name: String) extends AST(s"$value.$name") {
      override def names = value.names
    }

    sealed abstract class Lit(s: String) extends AST(s) {
      override def names = Set()
    }
    case class LitInt(value: Int) extends Lit(value.toString)

    case class Op(lhs: AST, op: String, rhs: AST) extends AST(s"$lhs $op $rhs") {
      override def names = lhs.names ++ rhs.names
    }
  }
}
