package com.todesking.prety

import com.todesking.prety.util.uniqueMap

object Lang {
  def parse(s: Seq[String]): Map[String, Def] =
    uniqueMap(s.map(parseSingle).flatten)
  def parseSingle(s: String): Map[String, Def] =
    Parser.parseAll(Parser.all, s) match {
      case Parser.NoSuccess(msg, _) =>
        throw new RuntimeException(s"Parse error($msg): $s")
      case Parser.Success(defs, _) =>
        defs
    }

  object Parser extends scala.util.parsing.combinator.RegexParsers {
    def all: Parser[Map[String, Def]] = repsep(pred, ",").map(uniqueMap)

    def pred = ((name <~ ':') ~ props) ^^ { case n ~ ps => (n, Def(uniqueMap(ps))) }
    def props = props_full | props_one
    def props_full = ('{' ~> repsep(prop, ",")) <~ '}'
    def props_one = expr ^^ { e => Seq("_" -> e) }
    def prop = name ~ (':' ~> expr) ^^ { case n ~ p => (n, p) }
    def expr = expr1 ~ (op ~ expr1).? ^^ {
      case lhs ~ Some(op ~ rhs) => Expr.Op(lhs, op, rhs)
      case lhs ~ None => lhs
    }
    def expr1 = measure | lit
    def measure = (the_value | ident) ~ rep('.' ~> name) ^^ {
      case id ~ names =>
        names.foldLeft[Expr](id) { (m, id) => Expr.Select(m, id) }
    }
    def ident = name ^^ { id => Expr.Ident(id) }
    def the_value = "_" ^^ { _ => Expr.TheValue }
    def name = "[a-zA-Z_][a-zA-Z_0-9]*".r
    def op = "[-+<>:*/]+".r
    def lit = int
    def int = "[0-9]|[1-9][0-9]*".r ^^ { v => Expr.LitInt(v.toInt) }
  }

  case class Def(props: Map[String, Expr])

  sealed abstract class Expr(override val toString: String) {
    def names: Set[String]
  }
  object Expr {
    case object TheValue extends Expr("_") {
      override def names = Set()
    }
    case class Ident(name: String) extends Expr(name) {
      override def names = Set(name)
    }
    case class Select(value: Expr, name: String) extends Expr(s"$value.$name") {
      override def names = value.names
    }

    sealed abstract class Lit(s: String) extends Expr(s) {
      override def names = Set()
    }
    case class LitInt(value: Int) extends Lit(value.toString)

    case class Op(lhs: Expr, op: String, rhs: Expr) extends Expr(s"$lhs $op $rhs") {
      override def names = lhs.names ++ rhs.names
    }
  }
}
