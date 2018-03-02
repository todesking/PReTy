package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Worlds { self: ForeignTypes with Queries with Preds with Props with Exprs with Conflicts =>
  trait World {
    val tpe: TypeSym
    def buildPred(expr: Expr): PropPred
    def solveConstraint(lhs: PropPred, rhs: PropPred): (Seq[Logic], Seq[Conflict])
  }

  class IntWorld extends World {
    override val tpe = query.types.int

    override def buildPred(expr: Expr): CorePred = expr match {
      case e: CoreExpr => CorePred(e)
    }

    override def solveConstraint(lhs: PropPred, rhs: PropPred) = (lhs, rhs) match {
      case (CorePred(l), CorePred(r)) =>
        (Seq(toLogic(l) --> toLogic(r)), Seq())
    }

    private[this] val E = CoreExpr
    private[this] def toLogic(e: CoreExpr): Logic = ???
  }

}
