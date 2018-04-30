package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Props { self: ForeignTypes with Values with Preds with Exprs with Conflicts with Envs with Debugging =>
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

  private[this] var prop2l = Map[(Value.Naked, PropKey), Logic]()
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
  def propFromLogic(l: Logic): Option[(Value, PropKey)] = l match {
    case lvar: Logic.Var =>
      Some(prop2l.find { case (k, v) => v == lvar }.get._1)
    case Logic.IValue(_) | Logic.BValue(_) =>
      None
    case other =>
      throw new RuntimeException(s"$other")
  }

  def propInLogicI(value: Value, prop: PropKey): Logic.LInt =
    propInLogic(value, prop) match {
      case v: Logic.LInt => v
      case other =>
        throw new RuntimeException(s"$other")
    }
  def propInLogicB(value: Value, prop: PropKey): Logic.LBool =
    propInLogic(value, prop) match {
      case v: Logic.LBool => v
      case other =>
        throw new RuntimeException(s"$other")
    }

  trait Prop {
    val tpe: TypeSym
    def buildPred(src: String, expr: Expr): PropPred
    def solveConstraint(theValue: Value, key: PropKey, env: Env, binding: Map[Value.Naked, Pred], lhs: PropPred, rhs: PropPred): (Seq[Logic.LBool], Seq[Conflict])
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

    override def solveConstraint(theValue: Value, key: PropKey, env: Env, binding: Map[Value.Naked, Pred], lhs: PropPred, rhs: PropPred) = (lhs, rhs) match {
      case (CorePred(_, l), CorePred(_, r)) =>
        // TODO: Move env loic generation to Solver
        // TODO: check base type constraint
        val v = propInLogic(theValue, key)

        val logicL = compileB(l, v)
        val logicR = compileB(r, v)

        def getCoreExpr(p: PropPred) = p match { case CorePred(_, e) => e }

        dprint("BE Start")
        dprint(logicL, logicR)
        def buildEnvLogic(vars: Set[Logic.Var], skip: Set[Logic.Var]): Logic.LBool = {
          dprint("BE", vars, skip)
          (vars -- skip).toSeq.foldLeft((Logic.True: Logic.LBool, skip ++ vars)) {
            case ((al, askip), vl) =>
              propFromLogic(vl).fold {
                // vl represents literal
                (al, askip)
              } {
                case (value, pkey) =>
                  dprint(al, askip, vl, value, pkey, binding(value.naked))
                  // TODO: use correspond Prop to build logic
                  val l = compileB(getCoreExpr(binding(value.naked).prop(pkey)), vl)
                  val al2 = al & l & buildEnvLogic(l.vars, askip)
                  (al2, askip ++ al2.vars)
              }
          }._1
        }

        val envLogic: Logic.LBool = v match {
          case lvar: Logic.Var =>
            buildEnvLogic(logicL.vars ++ logicR.vars, Set(lvar))
          case Logic.BValue(_) | Logic.IValue(_) =>
            buildEnvLogic(logicL.vars ++ logicR.vars, Set())
          case other =>
            throw new RuntimeException(s"$other")
        }

        def condsToLogic(l: Map[Value.Naked, Pred], not: Boolean) =
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
        val condLogic = condsToLogic(binding.filterKeys(env.conds.map(_.naked)), not = false)
        val uncondLogic = condsToLogic(binding.filterKeys(env.unconds.map(_.naked)), not = true)

        (Seq((envLogic & condLogic & uncondLogic & logicL) --> logicR), Seq())

      case _ =>
        throw new RuntimeException(s"Unsupported pred pair: $lhs, $rhs")
    }

    private[this] val E = CoreExpr
    private[this] val L = Logic
    private[this] def compileB(e: CoreExpr, theValue: Logic): Logic.LBool = e match {
      case E.TheValue(_) =>
        theValue match {
          case v: Logic.LBool => v
          case other =>
            throw new RuntimeException(s"$other")
        }
      case E.INT_GT(l, r) =>
        compileI(l, theValue) > compileI(r, theValue)
      case E.INT_GE(l, r) =>
        compileI(l, theValue) >= compileI(r, theValue)
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
      case other =>
        throw new RuntimeException(s"$other")
    }
    private[this] def compileI(e: CoreExpr, theValue: Logic): Logic.LInt = e match {
      case E.TheValue(_) =>
        theValue match {
          case v: Logic.LInt => v
          case other =>
            throw new RuntimeException(s"$other")
        }
      case E.ValueRef(v) =>
        propInLogicI(v, PropKey.Self)
      case E.INT_Lit(x) =>
        Logic.IValue(x)
      case E.INT_DIV(l, r) =>
        Logic.Div(compileI(l, theValue), compileI(r, theValue))
      case E.INT_MUL(l, r) =>
        Logic.Mul(compileI(l, theValue), compileI(r, theValue))
      case E.INT_PLUS(l, r) =>
        Logic.Plus(compileI(l, theValue), compileI(r, theValue))
      case other =>
        throw new RuntimeException(s"$other")
    }
  }

  class IntProp extends CoreProp {
    override val tpe = query.types.int
  }

  class BooleanProp extends CoreProp {
    override val tpe = query.types.boolean
  }

}
