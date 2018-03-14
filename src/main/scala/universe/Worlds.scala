package com.todesking.prety.universe

import com.todesking.prety.{ Logic }

trait Worlds { self: ForeignTypes with Envs with ForeignTypeOps with Constraints with Queries with Values with Preds with Props with Exprs with Conflicts with Templates =>

  private[this] var nextVarId = 0
  def freshVar(tpe: Logic.Type): Logic.Var = {
    val v = Logic.Var(nextVarId, tpe)
    nextVarId += 1
    v
  }

  private[this] var prop2l = Map[(Value, PropKey), Logic.Var]()
  def propInLogic(value: Value, prop: PropKey): Logic.Var =
    prop2l.get((value, prop)) getOrElse {
      val v = freshVar(logicType(prop.tpe))
      prop2l = prop2l + ((value, prop) -> v)
      v
    }

  private[this] lazy val T = query.types
  def logicType(tpe: TypeSym): Logic.Type =
    if (tpe <:< T.int) Logic.TInt
    else if (tpe <:< T.boolean) Logic.TBool
    else throw new RuntimeException(s"Theres no type $tpe in Logic")

  trait World {
    val tpe: TypeSym
    def buildPred(src: String, expr: Expr): PropPred
    def solveConstraint(env: Env, binding: Map[Value, Pred], lhs: PropPred, rhs: PropPred): (Seq[Logic], Seq[Conflict])
    // pred.tpe == this.tpe
    def toLogic(pred: PropPred, theValue: Value): Logic
  }

  abstract class CoreWorld extends World {
    override def buildPred(src: String, expr: Expr): CorePred = expr match {
      case e: CoreExpr => CorePred(src, e)
    }

    override def toLogic(pred: PropPred, theValue: Value): Logic = pred match {
      case CorePred(_, p) => compile(p, propInLogic(theValue, globalEnv.selfPropKey(theValue.tpe)))
    }

    override def solveConstraint(env: Env, binding: Map[Value, Pred], lhs: PropPred, rhs: PropPred) = (lhs, rhs) match {
      case (CorePred(_, l), CorePred(_, r)) =>
        // TODO: check base type constraint
        val v = freshVar(logicType(this.tpe))
        val envLogic = binding.filterKeys(env.values).flatMap {
          case (value, pred) =>
            // TODO: use corresponding world
            pred.definedProps.map {
              case (prop, ppred) =>
                this.toLogic(ppred, value)
            }
        }.reduceOption(_ & _) getOrElse Logic.True
        val condLogic =
          binding.filterKeys(env.conds).flatMap {
            case (value, pred) =>
              pred.definedProps.map {
                case (prop, ppred) =>
                  this.toLogic(ppred, value)
              }
          }.reduceOption(_ & _) getOrElse Logic.True
        val uncondLogic =
          binding.filterKeys(env.unconds).flatMap {
            case (value, pred) =>
              pred.definedProps.map {
                case (prop, ppred) =>
                  !this.toLogic(ppred, value): Logic
              }
          }.reduceOption(_ & _) getOrElse Logic.True
        (Seq((envLogic & condLogic & uncondLogic & compile(l, v)) --> compile(r, v)), Seq())
      case _ =>
        throw new RuntimeException(s"Unsupported pred pair: $lhs, $rhs")
    }

    private[this] val E = CoreExpr
    private[this] val L = Logic
    private[this] def compile(e: CoreExpr, theValue: Logic.Var): Logic = e match {
      case E.TheValue(_) =>
        theValue
      case E.ValueRef(v) =>
        propInLogic(v, globalEnv.selfPropKey(v.tpe))
      case E.INT_Lit(x) =>
        Logic.IntValue(x)
      case E.INT_GT(l, r) =>
        compile(l, theValue) > compile(r, theValue)
      case E.INT_LT(l, r) =>
        compile(l, theValue) < compile(r, theValue)
      case E.INT_EQ(l, r) =>
        compile(l, theValue) === compile(r, theValue)
      case E.BOOL_Lit(v) =>
        L.BoolValue(v)
      case E.And(es) =>
        es.map(compile(_, theValue)).reduce(_ & _)
    }
  }

  class IntWorld extends CoreWorld {
    override val tpe = query.types.int
  }

  class BooleanWorld extends CoreWorld {
    override val tpe = query.types.boolean
  }

}
