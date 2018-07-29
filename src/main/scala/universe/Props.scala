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

  // TODO: OH dirty
  private[this] lazy val vInt = Value.Origin(-1, "_", query.types.int)
  private[this] lazy val vBool = Value.Origin(-2, "_", query.types.boolean)

  private[this] var prop2l = Map[(Value.Naked, Seq[PropKey]), Logic]()
  def propInLogic(value: Value, path: Seq[PropKey]): Logic =
    // TODO: [BUG] support path
    prop2l.get((value.naked, path)) getOrElse {
      val naked = value.naked
      val logic =
        naked match {
          case Value.IntLiteral(l) =>
            Logic.IValue(l)
          case Value.BooleanLiteral(l) =>
            Logic.BValue(l)
          case _ =>
            val t = naked.tpe
            if (t <:< query.types.int) freshIVar(naked.shortString)
            else if (t <:< query.types.boolean) freshBVar(naked.shortString)
            else freshIVar(naked.shortString) // TODO: use identity type if supported
        }
      prop2l = prop2l + ((naked, path) -> logic)
      logic
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
    def solveConstraint(path: Seq[PropKey], env: Logic.LBool, lhs: Expr, rhs: Expr): Either[Seq[Conflict], Logic.LBool]
    // pred.tpe == this.tpe
    // pred(theValue) --> result
    def toLogic(pred: Expr, theValue: Value): Logic.LBool
  }

  abstract class CoreProp extends Prop {
    override def toLogic(pred: Expr, theValue: Value): Logic.LBool = pred match {
      case e: CoreExpr => compileB(e, propInLogic(theValue, Seq()))
    }

    override def solveConstraint(path: Seq[PropKey], env: Logic.LBool, lhs: Expr, rhs: Expr) = (lhs, rhs) match {
      case (l: CoreExpr, r: CoreExpr) =>
        // TODO: check base type constraint
        val root =
          if (tpe == query.types.int) vInt
          else if (tpe == query.types.boolean) vBool
          else ???
        val v = propInLogic(root, path)

        val logicL = compileB(l, v)
        val logicR = compileB(r, v)
        Right((env & logicL) --> logicR)
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
  class AnyRefProp extends CoreProp {
    override val tpe = query.types.anyRef
  }

}
