package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Props { self: ForeignTypes with Values with Preds with Exprs with Conflicts with Envs =>
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
  def freshIVar(name: String): Logic.IVar = {
    val v = Logic.IVar(nextVarId, name)
    nextVarId += 1
    v
  }
  def freshBVar(name: String): Logic.BVar = {
    val v = Logic.BVar(nextVarId, name)
    nextVarId += 1
    v
  }

  private[this] var prop2l = Map[(Value, PropKey), Logic]()
  def propInLogic(value: Value, prop: PropKey): Logic =
    prop2l.get((value.naked, prop)) getOrElse {
      val naked = value.naked
      val v = naked match {
        case Value.IntLiteral(i) if prop == PropKey.Self => // TODO
          Logic.IValue(i)
        case _ =>
          val t = prop.typeFor(naked.tpe)
          if (t <:< query.types.int) freshIVar(naked.toString)
          else if (t <:< query.types.boolean) freshBVar(naked.toString)
          else throw new RuntimeException(s"Theres no type $t in Logic")
      }
      prop2l = prop2l + ((naked, prop) -> v)
      v
    }
  def propInLogicI(value: Value, prop: PropKey): Logic.LInt =
    propInLogic(value, prop) match {
      case v: Logic.LInt => v
    }
  def propInLogicB(value: Value, prop: PropKey): Logic.LBool =
    propInLogic(value, prop) match {
      case v: Logic.LBool => v
    }

  trait Prop {
    val tpe: TypeSym
    def buildPred(src: String, expr: Expr): PropPred
    def solveConstraint(theValue: Value, key: PropKey, env: Env, binding: Map[Value, Pred], lhs: PropPred, rhs: PropPred): (Seq[Logic.LBool], Seq[Conflict])
    // pred.tpe == this.tpe
    def toLogic(pred: PropPred, theValue: Value): Logic.LBool
  }

  abstract class CoreProp extends Prop {
    override def buildPred(src: String, expr: Expr): CorePred = expr match {
      case e: CoreExpr => CorePred(src, e)
    }

    override def toLogic(pred: PropPred, theValue: Value): Logic.LBool = pred match {
      case CorePred(_, p) => compileB(p, propInLogic(theValue, PropKey.Self))
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
        def condsToLogic(l: Map[Value, Pred], not: Boolean) =
          l.flatMap {
            case (value, pred) =>
              pred.definedProps.map {
                case (prop, ppred) =>
                  if (not) {
                    this.toLogic(ppred, value) & !propInLogicB(value, prop)
                  } else {
                    this.toLogic(ppred, value) & propInLogicB(value, prop)
                  }
              }
          }.reduceOption(_ & _) getOrElse Logic.True
        val condLogic = condsToLogic(binding.filterKeys(env.conds), not = false)
        val uncondLogic = condsToLogic(binding.filterKeys(env.unconds), not = true)
        (Seq((envLogic & condLogic & uncondLogic & compileB(l, v)) --> compileB(r, v)), Seq())
      case _ =>
        throw new RuntimeException(s"Unsupported pred pair: $lhs, $rhs")
    }

    private[this] val E = CoreExpr
    private[this] val L = Logic
    private[this] def compileB(e: CoreExpr, theValue: Logic): Logic.LBool = e match {
      case E.TheValue(_) =>
        theValue match {
          case v: Logic.LBool => v
        }
      case E.INT_GT(l, r) =>
        compileI(l, theValue) > compileI(r, theValue)
      case E.INT_LT(l, r) =>
        compileI(l, theValue) < compileI(r, theValue)
      case E.INT_EQ(l, r) =>
        compileI(l, theValue) === compileI(r, theValue)
      case E.BOOL_Lit(v) =>
        L.BValue(v)
      case E.BOOL_EQ(l, r) =>
        compileB(l, theValue) === compileB(r, theValue)
      case E.And(es) =>
        es.map(compileB(_, theValue)).reduce(_ & _)
    }
    private[this] def compileI(e: CoreExpr, theValue: Logic): Logic.LInt = e match {
      case E.TheValue(_) =>
        theValue match {
          case v: Logic.LInt => v
        }
      case E.ValueRef(v) =>
        propInLogicI(v, PropKey.Self)
      case E.INT_Lit(x) =>
        Logic.IValue(x)
    }
  }

  class IntProp extends CoreProp {
    override val tpe = query.types.int
  }

  class BooleanProp extends CoreProp {
    override val tpe = query.types.boolean
  }

}
