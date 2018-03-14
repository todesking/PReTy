package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Props { self: ForeignTypes with Queries with ForeignTypeOps with Values with Preds with Exprs with Conflicts with Envs =>
  sealed abstract class PropKey {
    def isTarget(t: TypeSym): Boolean
    def typeFor(t: TypeSym): TypeSym
    def name: String
  }
  object PropKey {
    case class Named(name: String, targetType: TypeSym, tpe: TypeSym) extends PropKey {
      require(name != "_")

      override def toString = s"[property $name: $targetType => $tpe]"
      override def isTarget(t: TypeSym) = t <:< targetType
      override def typeFor(t: TypeSym) = tpe
    }
    case object Self extends PropKey {
      override def toString = s"[property self]"
      override def isTarget(t: TypeSym) = true
      // TODO: clean literal types
      override def typeFor(t: TypeSym) = t
      override def name = "_"
    }
  }

  private[this] var nextVarId = 0
  def freshVar(tpe: Logic.Type, name: String): Logic.Var = {
    val v = Logic.Var(nextVarId, tpe, name)
    nextVarId += 1
    v
  }

  private[this] var prop2l = Map[(Value, PropKey), Logic]()
  def propInLogic(value: Value, prop: PropKey): Logic =
    prop2l.get((value.naked, prop)) getOrElse {
      val naked = value.naked
      val v = naked match {
        case Value.IntLiteral(i) if prop == PropKey.Self => // TODO
          Logic.IntValue(i)
        case _ =>
          freshVar(logicType(prop.typeFor(naked.tpe)), naked.toString)
      }
      prop2l = prop2l + ((naked, prop) -> v)
      v
    }

  private[this] lazy val T = query.types
  def logicType(tpe: TypeSym): Logic.Type =
    if (tpe <:< T.int) Logic.TInt
    else if (tpe <:< T.boolean) Logic.TBool
    else throw new RuntimeException(s"Theres no type $tpe in Logic")

  trait Prop {
    val tpe: TypeSym
    def buildPred(src: String, expr: Expr): PropPred
    def solveConstraint(theValue: Value, key: PropKey, env: Env, binding: Map[Value, Pred], lhs: PropPred, rhs: PropPred): (Seq[Logic], Seq[Conflict])
    // pred.tpe == this.tpe
    def toLogic(pred: PropPred, theValue: Value): Logic
  }

  abstract class CoreProp extends Prop {
    override def buildPred(src: String, expr: Expr): CorePred = expr match {
      case e: CoreExpr => CorePred(src, e)
    }

    override def toLogic(pred: PropPred, theValue: Value): Logic = pred match {
      case CorePred(_, p) => compile(p, propInLogic(theValue, PropKey.Self))
    }

    override def solveConstraint(theValue: Value, key: PropKey, env: Env, binding: Map[Value, Pred], lhs: PropPred, rhs: PropPred) = (lhs, rhs) match {
      case (CorePred(_, l), CorePred(_, r)) =>
        // TODO: check base type constraint
        val v = propInLogic(theValue, key)
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
    private[this] def compile(e: CoreExpr, theValue: Logic): Logic = e match {
      case E.TheValue(_) =>
        theValue
      case E.ValueRef(v) =>
        propInLogic(v, PropKey.Self)
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

  class IntProp extends CoreProp {
    override val tpe = query.types.int
  }

  class BooleanProp extends CoreProp {
    override val tpe = query.types.boolean
  }

}
