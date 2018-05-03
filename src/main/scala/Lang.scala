package com.todesking.prety

import com.todesking.prety.util.uniqueMap

object Lang {
  def parse(s: Seq[String]): Map[String, Pred] =
    uniqueMap(s.map(parseSingle).flatten)
  def parseSingle(s: String): Map[String, Pred] =
    Parser.parseAll(Parser.all, s) match {
      case Parser.NoSuccess(msg, _) =>
        throw new RuntimeException(s"Parse error($msg): $s")
      case Parser.Success(defs, _) =>
        defs
    }
  def parseExpr(s: String): Expr =
    Parser.parseAll(Parser.expr, s) match {
      case Parser.NoSuccess(msg, _) =>
        throw new RuntimeException(s"Parse error($msg): $s")
      case Parser.Success(e, _) =>
        e
    }

  object Parser extends scala.util.parsing.combinator.RegexParsers {
    private[this] def surround[A](l: String, content: => Parser[A], r: String): Parser[A] =
      (l ~> content) <~ r

    private[this] def sep[L, R](l: Parser[L], s: String, r: Parser[R]): Parser[L ~ R] =
      (l <~ s) ~ r

    private[this] def list[A](p: Parser[A]): Parser[Seq[A]] =
      repsep(p, ",")

    private[this] def kv[K, V](k: Parser[K], v: => Parser[V]): Parser[(K, V)] =
      sep(k, ":", v) ^^ { case a ~ b => (a, b) }

    private[this] def dict[K, V](k: Parser[K], v: => Parser[V]): Parser[Map[K, V]] =
      surround("{", list(kv(k, v)), "}") ^^ uniqueMap

    def all: Parser[Map[String, Pred]] = list(kv(name, pred)) ^^ uniqueMap

    def pred: Parser[Pred] = pred_full | pred_one
    def pred_full = dict(name, pred) ^^ { ps =>
      // TODO: handle {_: {v1: ...}, v2: ...} pattern(should error?)
      Pred(ps.get("_").map(_.self) getOrElse Expr.LitBoolean(true), ps.filterKeys(_ != "_"))
    }
    def pred_one = expr ^^ { e => Pred(e, Map()) }

    def expr: Parser[Expr] = opapp | expr1
    def expr1 = group | atom
    def opapp: Parser[Expr] = expr1 ~ op ~ expr1 ^^ {
      case lhs ~ op ~ rhs => Expr.Op(lhs, op, rhs)
    }
    def group: Parser[Expr] = ("(" ~> expr) <~ ")"
    def atom = lit | ((ref ~ app.?) ^^ {
      case e ~ Some(args) => Expr.App(e, args)
      case e ~ None => e
    })
    def ref = (the_value | ident | makro) ~ rep("." ~> name) ^^ {
      case id ~ names =>
        names.foldLeft[Expr](id) { (m, id) => Expr.Select(m, id) }
    }
    def makro = "@" ~> name ^^ { id => Expr.MacroRef(id) }
    def app: Parser[Seq[Expr]] = ("(" ~> repsep(expr, ",")) <~ ")"
    def ident = name ^^ { id => Expr.Ident(id) }
    def the_value = "_" ^^ { _ => Expr.TheValue }
    def name = "[a-zA-Z_][a-zA-Z_0-9]*".r
    def op = "[-+<>:*/=]+".r
    def lit = int | bool
    def int = "0|[1-9][0-9]*".r ^^ { v => Expr.LitInt(v.toInt) }
    def bool = ("true" | "false") ^^ {
      case "true" => Expr.LitBoolean(true)
      case "false" => Expr.LitBoolean(false)
    }
  }

  case class Pred(self: Expr, props: Map[String, Pred])

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
    case class LitBoolean(value: Boolean) extends Lit(value.toString)

    case class Op(lhs: Expr, op: String, rhs: Expr) extends Expr(s"$lhs $op $rhs") {
      override def names = lhs.names ++ rhs.names
    }

    case class MacroRef(name: String) extends Expr(s"@$name") {
      override def names = Set()
    }

    case class App(expr: Expr, args: Seq[Expr]) extends Expr(s"$expr(${args.mkString(", ")})") {
      override def names = expr.names ++ args.flatMap(_.names)
    }
  }
}
