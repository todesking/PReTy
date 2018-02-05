package com.todesking.prety.scalac_plugin.universe

trait Preds { self: ForeignTypes with ASTs =>
  sealed abstract class Result[A] {
    def map[B](f: A => B): Result[B]
    def flatMap[B](f: A => Result[B]): Result[B]
    def foreach(f: A => Unit): Unit
  }
  object Result {
    case class Success[A](value: A) extends Result[A] {
      override def map[B](f: A => B): Result[B] = Success(f(value))
      override def flatMap[B](f: A => Result[B]): Result[B] = f(value)
      override def foreach(f: A => Unit): Unit = f(value)
    }
  }

  class PredEnv {
    def lookupMacro(name: String): Option[PredMacro] = ???
    def lookupPropId(name: String): Option[PropId] = ???
    def lookupOp(name: String, l: PredType, r: PredType): Result[(Logic, Logic) => Logic] = ???
  }

  sealed abstract class PredMacro {
    def apply(env: PredEnv, args: Seq[Lang.AST]): Lang.AST
    def prop(id: PropId): Lang.AST
    def eval(): Lang.AST
  }
  object PredMacro {
  }

  sealed abstract class Pred {
    def eval(env: PredEnv): Logic = ???
  }
  object Pred {
    def and(ps: Seq[Pred]): Pred = if (ps.isEmpty) True else And(ps)
    case object True extends Pred
    case object False extends Pred
    case class And(preds: Seq[Pred]) extends Pred
    case class Expr(expr: Lang.AST, env: Map[String, Value]) extends Pred
  }

  abstract class PredType {
  }
  object PredType {
    case class L0(tpe: TypeSym) extends PredType
    case class L1(tpe: scala.reflect.runtime.universe.Type) extends PredType
    case class Fun(params: Seq[PredType], ret: PredType) extends PredType
    case object Int extends PredType
  }

  case class PropId(id: String, targetType: PredType, tpe: PredType)

  sealed abstract class Logic {
    def ====(rhs: Logic) = Logic.Eq(this, rhs)
    def <=(rhs: Logic) = Logic.Le(this, rhs)
    def <(rhs: Logic) = Logic.Lt(this, rhs)
    def >=(rhs: Logic) = Logic.Ge(this, rhs)
    def >(rhs: Logic) = Logic.Gt(this, rhs)

    def +(rhs: Logic) = Logic.Plus(this, rhs)
    def -(rhs: Logic) = Logic.Minus(this, rhs)
    def unary_- = Logic.Neg(this)

    def unary_! = Logic.Not(this)
    def &(rhs: Logic) = Logic.And(Seq(this, rhs))
    def |(rhs: Logic) = Logic.Or(Seq(this, rhs))
    def -->(rhs: Logic) = Logic.Implie(this, rhs)
  }
  object Logic {
    sealed abstract class Type
    case object TInt extends Type
    case object TBool extends Type
    case object TString extends Type
    case class TTuple(elms: Seq[Type]) extends Type

    case class IntValue(value: Int) extends Logic
    case class BoolValue(value: Boolean) extends Logic
    case class StringValue(value: String) extends Logic

    val True = BoolValue(true)
    val False = BoolValue(false)

    case class App(fun: String, args: Seq[Logic]) extends Logic

    case class Eq(lhs: Logic, rhs: Logic) extends Logic
    case class Lt(lhs: Logic, rhs: Logic) extends Logic
    case class Le(lhs: Logic, rhs: Logic) extends Logic
    case class Gt(lhs: Logic, rhs: Logic) extends Logic
    case class Ge(lhs: Logic, rhs: Logic) extends Logic

    case class Neg(expr: Logic) extends Logic
    case class Plus(lhs: Logic, rhs: Logic) extends Logic
    case class Minus(lhs: Logic, rhs: Logic) extends Logic

    case class Not(expr: Logic) extends Logic
    case class And(exprs: Seq[Logic]) extends Logic
    case class Or(exprs: Seq[Logic]) extends Logic
    case class Implie(lhs: Logic, rhs: Logic) extends Logic
  }

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

    sealed abstract class AST
    object AST {
      case object TheValue extends AST
      case class Ident(name: String) extends AST
      case class Select(value: AST, name: String) extends AST

      sealed abstract class Lit extends AST
      case class LitInt(value: Int) extends Lit

      case class Op(lhs: AST, op: String, rhs: AST) extends AST
    }
  }
}
