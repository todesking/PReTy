package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Exprs { self: ForeignTypes with Queries with Values with Envs =>
  abstract class Expr {
    def tpe: TypeSym
    def substitute(mapping: Map[Value, Value]): Expr
    def messageString: String
  }
  object Expr {
    import Lang.{ Expr => E }
    val CE = CoreExpr
    def compile(ast: Lang.Expr, env: Env, theType: TypeSym): Expr = ast match {
      case E.TheValue =>
        CE.TheValue(theType)
      case E.Ident(name) =>
        CE.ValueRef(env.findValue(name))
      case E.Select(expr, name) =>
        ???
      case E.LitInt(value) =>
        CE.INT_Lit(value)
      case E.Op(lhs, op, rhs) =>
        val l = compile(lhs, env, theType)
        val r = compile(rhs, env, theType)
        env.findOp(l.tpe, op).apply(l, r)
    }
  }
  sealed abstract class CoreExpr extends Expr {
    override def substitute(mapping: Map[Value, Value]): CoreExpr
    def children: Seq[CoreExpr]
    override def messageString = toString
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

    case class TheValue(tpe: TypeSym) extends Leaf {
      override def substitute(mapping: Map[Value, Value]) = this
      override def toString = s"_"
    }
    case class ValueRef(value: Value) extends Leaf {
      override def tpe = value.tpe
      override def substitute(mapping: Map[Value, Value]) =
        mapping.get(value).map(ValueRef.apply) getOrElse this
      override def toString = s"ref($value)"
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
  }
}
