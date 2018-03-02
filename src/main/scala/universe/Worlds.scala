package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Worlds { self: ForeignTypes with ForeignTypeOps with Queries with Values with Preds with Props with Exprs with Conflicts =>
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
        // TODO: check base type constraint
        val v = freshVar(r.tpe)
        (Seq(toLogic(l, v) --> toLogic(r, v)), Seq())
      case _ =>
        throw new RuntimeException(s"Unsupported pred pair: $lhs, $rhs")
    }

    private[this] val E = CoreExpr
    private[this] val L = Logic
    private[this] def toLogic(e: CoreExpr, theValue: Logic.Var): Logic = e match {
      case E.TheValue(_) =>
        theValue
      case E.ValueRef(v) =>
        valueInLogic(v)
      case E.INT_Lit(x) =>
        Logic.IntValue(x)
      case E.INT_GT(l, r) =>
        toLogic(l, theValue) > toLogic(r, theValue)
      case E.INT_EQ(l, r) =>
        toLogic(l, theValue) === toLogic(r, theValue)
      case E.BOOL_Lit(v) =>
        L.BoolValue(v)
    }

    private[this] var nextVarId = 1
    private[this] def freshVar(tpe: TypeSym): Logic.Var = {
      val v = Logic.Var(nextVarId, logicType(tpe))
      nextVarId += 1
      v
    }

    private[this] val T = query.types
    private[this] def logicType(tpe: TypeSym): Logic.Type =
      if (tpe <:< T.int) Logic.TInt
      else if (tpe <:< T.boolean) Logic.TBool
      else throw new RuntimeException(s"Theres no type $tpe in Logic")

    private[this] var vil = Map.empty[Value, Logic.Var]
    private[this] def valueInLogic(v: Value): Logic.Var =
      vil.get(v) getOrElse {
        val lv = freshVar(v.tpe)
        vil = vil + (v -> lv)
        lv
      }
  }

}
