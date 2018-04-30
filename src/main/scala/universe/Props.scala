package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Props { self: ForeignTypes with Values with Preds with Exprs with Conflicts with Envs with Debugging =>
  case class PropKey(name: String, targetType: TypeSym, tpe: TypeSym) {
    def isTarget(t: TypeSym) = tpe <:< targetType
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

  private[this] var prop2l = Map[(Value.Naked, Seq[PropKey]), Logic]()
  def propInLogic(value: Value, path: Seq[PropKey]): Logic =
    // TODO: [BUG] support path
    prop2l.get((value.naked, path)) getOrElse {
      val naked = value.naked
      val v = {
        val t = naked.tpe
        if (t <:< query.types.int) freshIVar(naked.toString)
        else if (t <:< query.types.boolean) freshBVar(naked.toString)
        else throw new RuntimeException(s"Theres no type $t in Logic")
      }
      prop2l = prop2l + ((naked, path) -> v)
      v
    }
  def propFromLogic(l: Logic): Option[(Value, Seq[PropKey])] = l match {
    case lvar: Logic.Var =>
      Some(prop2l.find { case (k, v) => v == lvar }.get._1)
    case Logic.IValue(_) | Logic.BValue(_) =>
      None
    case other =>
      throw new RuntimeException(s"$other")
  }

  def propInLogicI(value: Value, path: Seq[PropKey]): Logic.LInt =
    propInLogic(value, path) match {
      case v: Logic.LInt => v
      case other =>
        throw new RuntimeException(s"$other")
    }
  def propInLogicB(value: Value, path: Seq[PropKey]): Logic.LBool =
    propInLogic(value, path) match {
      case v: Logic.LBool => v
      case other =>
        throw new RuntimeException(s"$other")
    }

  trait Prop {
    val tpe: TypeSym
    def solveConstraint(theValue: Value, key: PropKey, env: Env, binding: Map[Value.Naked, Pred], lhs: Expr, rhs: Expr): (Seq[Logic.LBool], Seq[Conflict])
    // pred.tpe == this.tpe
    def toLogic(pred: Expr, theValue: Value): Logic.LBool
  }

  abstract class CoreProp extends Prop {
    override def toLogic(pred: Expr, theValue: Value): Logic.LBool = pred match {
      case e: CoreExpr => compileB(e, propInLogic(theValue, Seq()))
    }

    override def solveConstraint(theValue: Value, key: PropKey, env: Env, binding: Map[Value.Naked, Pred], lhs: Expr, rhs: Expr) = (lhs, rhs) match {
      case (l: CoreExpr, r: CoreExpr) =>
        // TODO: Move env loic generation to Solver
        // TODO: check base type constraint
        val v = propInLogic(theValue, Seq(key))

        val logicL = compileB(l, v)
        val logicR = compileB(r, v)

        def getCoreExpr(e: Expr) = e match { case e: CoreExpr => e }

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
                case (value, path) =>
                  dprint(al, askip, vl, value, path, binding(value.naked))
                  // TODO: use correspond Prop to build logic
                  val pred = path match {
                    case Seq() => binding(value.naked)
                    case Seq(k) => binding(value.naked).prop(k)
                  }
                  val l = compileB(getCoreExpr(pred.self), vl)
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
          // TODO: [BUG]
          l.flatMap {
            case (value, pred) =>
              pred.definedProps.map {
                case (prop, ppred) =>
                  if (not) {
                    this.toLogic(ppred.self, value) & !propInLogicB(value, Seq(prop))
                  } else {
                    this.toLogic(ppred.self, value) & propInLogicB(value, Seq(prop))
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
        propInLogicI(v, Seq())
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
