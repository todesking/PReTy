package com.todesking.prety.universe

trait Props { self: ForeignTypes with Queries with ForeignTypeOps with Values =>
  case class PropKey(name: String, targetType: TypeSym, tpe: TypeSym)

  trait PropExpr {
    def tpe: TypeSym
    def toPropPred: PropPred = PropPred(this)
  }
  case class PropPred(expr: PropExpr) {
    require(expr.tpe <:< query.types.boolean)
    def tpe = query.types.boolean
  }

  object PropExpr {
    case class ValueRef(value: Value) extends PropExpr {
      override def tpe = value.tpe
    }
    case class LitInt(value: Int) extends PropExpr {
      override def tpe = query.types.int
    }
  }
}
