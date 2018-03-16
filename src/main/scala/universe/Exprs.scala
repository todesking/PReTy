package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Exprs { self: ForeignTypes with Values with Envs with Worlds with Macros with Templates =>
  abstract class Expr {
    def tpe: TypeSym
    def substitute(mapping: Map[Value, Value]): Expr
    def messageString: String
  }
  object Expr {
    import Lang.{ Expr => E }
    val CE = CoreExpr
    def compile(w: World, ast: Lang.Expr, env: Env, theType: TypeSym): Expr =
      compile(w, ast, env, theType, Map.empty)
    def compile(w: World, ast: Lang.Expr, env: Env, theType: TypeSym, sub: Map[String, Expr]): Expr =
      compile0(w, ast, env, theType, sub) match {
        case Right(e) => e
        case Left(m) => m.expr
      }
    private[this] def compile0(w: World, ast: Lang.Expr, env: Env, theType: TypeSym, sub: Map[String, Expr]): Either[Macro, Expr] = ast match {
      case E.TheValue =>
        Right(CE.TheValue(theType))
      case E.Ident(name) =>
        if (sub.contains(name)) Right(sub(name))
        else Right(CE.ValueRef(env.findValue(name)))
      case E.LitInt(value) =>
        Right(CE.INT_Lit(value))
      case E.Op(lhs, op, rhs) =>
        val l = compile(w, lhs, env, theType, sub)
        val r = compile(w, rhs, env, theType, sub)
        val m = w.findMethodMacro(l.tpe, op, Seq(Seq(r.tpe)))
        m.apply(Some(l), Seq(r), env)
      case E.MacroRef(name) =>
        Left(w.findMacro(name))
      case E.Select(expr, name) =>
        compile0(w, expr, env, theType, sub) match {
          case Left(m) => m.select(name)
          case Right(e) =>
            throw new RuntimeException(s"Invalid select: $ast")
        }
      case E.App(expr, args) =>
        compile0(w, expr, env, theType, sub) match {
          case Left(m) =>
            m.apply(None, args.map(compile(w, _, env, theType, sub)), env)
          case Right(e) =>
            throw new RuntimeException(s"Invalid app: $ast")
        }
    }
  }
  sealed abstract class CoreExpr extends Expr {
    override def substitute(mapping: Map[Value, Value]): CoreExpr
    def children: Seq[CoreExpr]
    override def messageString = toString
    def &(rhs: CoreExpr) = rhs match {
      case CoreExpr.And(es) =>
        CoreExpr.And(this +: es)
      case e =>
        CoreExpr.And(Seq(this, e))
    }
  }
  object CoreExpr {
    import query.{ types => T }
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
      override def &(rhs: CoreExpr) = And(es :+ rhs)
    }

    case class TheValue(tpe: TypeSym) extends Leaf {
      override def substitute(mapping: Map[Value, Value]) = this
      override def toString = s"_"
    }
    case class ValueRef(value: Value) extends Leaf {
      override def tpe = value.tpe
      override def substitute(mapping: Map[Value, Value]) =
        mapping.get(value).map(ValueRef.apply) getOrElse this
      override def toString = value.toString
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
