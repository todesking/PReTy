package com.todesking.prety.scalac_plugin.universe

trait Preds {
  sealed abstract class Pred
  object Pred {
    def and(ps: Seq[Pred]): Pred = ???
    case object True extends Pred
    case object False extends Pred
  }

  object Lang {
    def parse(s: String): Seq[AST.Pred] =
      Parser.parse(Parser.all, s).get

    object Parser extends scala.util.parsing.combinator.RegexParsers {
      def all = repsep(pred, ",")

      def pred = (ident <~ ':') ~ expr ^^ { case id ~ expr => AST.Pred(id.name, expr) }
      def expr = expr1 ~ (op ~ expr1).? ^^ {
        case lhs ~ Some(op ~ rhs) => AST.Op(lhs, op, rhs)
        case lhs ~ None => lhs
      }
      def expr1 = measure | lit
      def measure = (the_value | ident) ~ rep('.' ~> name) ^^ {
        case id ~ names =>
          names.foldLeft[AST.Measure](id) { (m, id) => AST.Select(m, id) }
      }
      def ident = name ^^ { id => AST.Ident(id) }
      def the_value = '_' ^^ { _ => AST.TheValue }
      def name = "[a-zA-Z_][a-zA-Z_0-9]*".r
      def op = "[-+<>:*/]+".r
      def lit = int
      def int = "[1-9][0-9]*".r ^^ { v => AST.LitInt(v.toInt) }
    }

    sealed abstract class AST
    object AST {
      case class Pred(id: String, expr: Expr) extends AST

      sealed abstract class Expr extends AST
      sealed abstract class Measure extends Expr

      case object TheValue extends Measure
      case class Ident(name: String) extends Measure
      case class Select(value: Measure, name: String) extends Measure

      sealed abstract class Lit extends Expr
      case class LitInt(value: Int) extends Lit

      case class Op(lhs: Expr, op: String, rhs: Expr) extends Expr
    }
  }
}
