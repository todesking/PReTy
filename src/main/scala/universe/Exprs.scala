package com.todesking.prety.universe

trait Exprs { self: ForeignTypes with Queries with Values =>
  abstract class Expr {
    def tpe: TypeSym
    def substitute(mapping: Map[Value, Value]): Expr
  }
  sealed abstract class CoreExpr extends Expr {
    override def substitute(mapping: Map[Value, Value]): CoreExpr
  }
  object CoreExpr {
    import query.{ types => T }
    case class TheValue(tpe: TypeSym) extends CoreExpr {
      override def substitute(mapping: Map[Value, Value]) = this
      override def toString = s"_: $tpe"
    }
    case class ValueRef(value: Value) extends CoreExpr {
      override def tpe = value.tpe
      override def substitute(mapping: Map[Value, Value]) =
        mapping.get(value).map(ValueRef.apply) getOrElse this
      override def toString = s"ref($value)"
    }

    case class INT_Lit(value: Int) extends CoreExpr {
      override def tpe = T.int
      override def substitute(mapping: Map[Value, Value]) = this
      override def toString = value.toString
    }
    case class INT_GT(lhs: CoreExpr, rhs: CoreExpr) extends CoreExpr {
      override def tpe = T.int
      override def substitute(mapping: Map[Value, Value]) =
        INT_GT(lhs.substitute(mapping), rhs.substitute(mapping))
      override def toString = s"$lhs > $rhs"
    }
    case class INT_EQ(lhs: CoreExpr, rhs: CoreExpr) extends CoreExpr {
      override def tpe = T.boolean
      override def substitute(mapping: Map[Value, Value]) =
        INT_GT(lhs.substitute(mapping), rhs.substitute(mapping))
      override def toString = s"$lhs == $rhs"
    }
    case class BOOL_Lit(value: Boolean) extends CoreExpr {
      override def tpe = T.boolean
      override def substitute(mapping: Map[Value, Value]) = this
      override def toString = value.toString
    }
  }
}
