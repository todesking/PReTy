package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Exprs { self: ForeignTypes with Values with Envs with Worlds with Macros with Templates =>
  abstract class Expr {
    def tpe: TypeSym
    def substitute(mapping: Map[Value, Value]): Expr
    def messageString: String
    def &(rhs: Expr): Expr
  }
  object Expr {
    import Lang.{ Expr => E }
    val CE = CoreExpr

    def compile(macroEnv: MacroEnv, env: Map[String, Expr], ast: Lang.Expr, theType: TypeSym): Expr =
      compile1(macroEnv, env, ast, theType) match {
        case Right(e) => e
        case Left(m) => m.expr(macroEnv)
      }
    private[this] def compile1(macroEnv: MacroEnv, env: Map[String, Expr], ast: Lang.Expr, theType: TypeSym): Either[Macro, Expr] = ast match {
      case E.TheValue =>
        Right(CE.TheValue(theType))
      case E.Ident(name) =>
        Right(env(name))
      case E.LitInt(value) =>
        Right(CE.INT_Lit(value))
      case E.LitBoolean(value) =>
        Right(CE.BOOL_Lit(value))
      case E.Op(lhs, op, rhs) =>
        val l = compile(macroEnv, env, lhs, theType)
        val r = compile(macroEnv, env, rhs, theType)
        val m = macroEnv.member(l.tpe, op, Seq(Seq(r.tpe))).getOrElse { throw new RuntimeException(s"Member macro not found: ${l.tpe}.$op(${r.tpe})") }
        m.apply(macroEnv, Some(l), Seq(r))
      case E.MacroRef(name) =>
        Left(macroEnv.global(name).getOrElse { throw new RuntimeException(s"Macro not found: $name") })
      case E.Select(expr, name) =>
        compile1(macroEnv, env, expr, theType) match {
          case Left(m) => m.select(macroEnv, name)
          case Right(e) =>
            throw new RuntimeException(s"Invalid select: $ast")
        }
      case E.App(expr, args) =>
        compile1(macroEnv, env, expr, theType) match {
          case Left(m) =>
            m.apply(macroEnv, None, args.map(compile(macroEnv, env, _, theType)))
          case Right(e) =>
            throw new RuntimeException(s"Invalid app: $ast")
        }
    }
  }
  sealed abstract class CoreExpr extends Expr {
    override def substitute(mapping: Map[Value, Value]): CoreExpr
    def children: Seq[CoreExpr]
    override def messageString = toString
    override def &(rhs: Expr) = rhs match {
      case CoreExpr.And(es) =>
        CoreExpr.And(this +: es)
      case e: CoreExpr =>
        this match {
          case CoreExpr.And(es) =>
            CoreExpr.And(es :+ e)
          case e2 =>
            CoreExpr.And(Seq(e2, e))
        }
    }
  }
  object CoreExpr {
    import query.{ types => T }

    val True = BOOL_Lit(true)
    val False = BOOL_Lit(false)

    sealed trait BinaryOp extends CoreExpr {
      def lhs: CoreExpr
      def rhs: CoreExpr
      override def children = Seq(lhs, rhs)
    }
    sealed trait Leaf extends CoreExpr {
      override def children = Seq()
    }

    case class And(es: Seq[CoreExpr]) extends CoreExpr {
      require(es.nonEmpty)

      override def children = es
      override val tpe = es.foldLeft(query.types.nothing) { (a, x) => if (a <:< x.tpe) x.tpe else a }
      override def substitute(mapping: Map[Value, Value]) =
        And(es.map(_.substitute(mapping)))
      override def toString =
        es.map(e => s"($e)").mkString(" & ")
    }

    case class TheValue(tpe: TypeSym) extends Leaf {
      override def substitute(mapping: Map[Value, Value]) = this
      override def toString = s"_"
    }
    case class ValueRef(value: Value) extends Leaf {
      override def tpe = value.tpe
      override def substitute(mapping: Map[Value, Value]) =
        mapping.get(value).map(ValueRef.apply) getOrElse this
      override def toString = value.shortString
      override def messageString = value.name
    }

    case class INT_Lit(value: Int) extends Leaf {
      override def tpe = T.int
      override def substitute(mapping: Map[Value, Value]) = this
      override def toString = value.toString
    }
    case class INT_GT(lhs: CoreExpr, rhs: CoreExpr) extends BinaryOp {
      override def tpe = T.boolean
      override def substitute(mapping: Map[Value, Value]) =
        INT_GT(lhs.substitute(mapping), rhs.substitute(mapping))
      override def toString = s"$lhs > $rhs"
    }
    case class INT_GE(lhs: CoreExpr, rhs: CoreExpr) extends BinaryOp {
      override def tpe = T.boolean
      override def substitute(mapping: Map[Value, Value]) =
        INT_GE(lhs.substitute(mapping), rhs.substitute(mapping))
      override def toString = s"$lhs >= $rhs"
    }
    case class INT_LT(lhs: CoreExpr, rhs: CoreExpr) extends BinaryOp {
      override def tpe = T.boolean
      override def substitute(mapping: Map[Value, Value]) =
        INT_LT(lhs.substitute(mapping), rhs.substitute(mapping))
      override def toString = s"$lhs < $rhs"
    }
    case class INT_EQ(lhs: CoreExpr, rhs: CoreExpr) extends BinaryOp {
      override def tpe = T.boolean
      override def substitute(mapping: Map[Value, Value]) =
        INT_EQ(lhs.substitute(mapping), rhs.substitute(mapping))
      override def toString = s"$lhs == $rhs"
    }
    case class INT_DIV(lhs: CoreExpr, rhs: CoreExpr) extends BinaryOp {
      override def tpe = T.int
      override def substitute(mapping: Map[Value, Value]) =
        INT_DIV(lhs.substitute(mapping), rhs.substitute(mapping))
      override def toString = s"$lhs / $rhs"
    }
    case class INT_MUL(lhs: CoreExpr, rhs: CoreExpr) extends BinaryOp {
      override def tpe = T.int
      override def substitute(mapping: Map[Value, Value]) =
        INT_MUL(lhs.substitute(mapping), rhs.substitute(mapping))
      override def toString = s"$lhs * $rhs"
    }
    case class INT_PLUS(lhs: CoreExpr, rhs: CoreExpr) extends BinaryOp {
      override def tpe = T.int
      override def substitute(mapping: Map[Value, Value]) =
        INT_DIV(lhs.substitute(mapping), rhs.substitute(mapping))
      override def toString = s"$lhs + $rhs"
    }
    case class BOOL_Lit(value: Boolean) extends Leaf {
      override def tpe = T.boolean
      override def substitute(mapping: Map[Value, Value]) = this
      override def toString = value.toString
    }
    case class BOOL_EQ(lhs: CoreExpr, rhs: CoreExpr) extends BinaryOp {
      override def tpe = T.boolean
      override def substitute(mapping: Map[Value, Value]) =
        BOOL_EQ(lhs.substitute(mapping), rhs.substitute(mapping))
      override def toString = s"$lhs == $rhs"
    }
  }
}
