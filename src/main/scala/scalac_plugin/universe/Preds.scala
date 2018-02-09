package com.todesking.prety.scalac_plugin.universe

trait Preds { self: ForeignTypes with ASTs =>

  abstract class PredType {
  }
  object PredType {
    case class L0(tpe: TypeSym) extends PredType
    case class L1(tpe: scala.reflect.runtime.universe.Type) extends PredType
    case class Fun(params: Seq[PredType], ret: PredType) extends PredType
    case object Int extends PredType
  }

  case class PropId(id: String, targetType: PredType, tpe: PredType)

  sealed abstract class Logic {
    def ====(rhs: Logic) = Logic.Eq(this, rhs)
    def <=(rhs: Logic) = Logic.Le(this, rhs)
    def <(rhs: Logic) = Logic.Lt(this, rhs)
    def >=(rhs: Logic) = Logic.Ge(this, rhs)
    def >(rhs: Logic) = Logic.Gt(this, rhs)

    def +(rhs: Logic) = Logic.Plus(this, rhs)
    def -(rhs: Logic) = Logic.Minus(this, rhs)
    def unary_- = Logic.Neg(this)

    def unary_! = Logic.Not(this)
    def &(rhs: Logic) = Logic.And(Seq(this, rhs))
    def |(rhs: Logic) = Logic.Or(Seq(this, rhs))
    def -->(rhs: Logic) = Logic.Implie(this, rhs)
  }
  object Logic {
    sealed abstract class Type
    case object TInt extends Type
    case object TBool extends Type
    case object TString extends Type
    case class TTuple(elms: Seq[Type]) extends Type

    case class IntValue(value: Int) extends Logic
    case class BoolValue(value: Boolean) extends Logic
    case class StringValue(value: String) extends Logic

    val True = BoolValue(true)
    val False = BoolValue(false)

    case class App(fun: String, args: Seq[Logic]) extends Logic

    case class Eq(lhs: Logic, rhs: Logic) extends Logic
    case class Lt(lhs: Logic, rhs: Logic) extends Logic
    case class Le(lhs: Logic, rhs: Logic) extends Logic
    case class Gt(lhs: Logic, rhs: Logic) extends Logic
    case class Ge(lhs: Logic, rhs: Logic) extends Logic

    case class Neg(expr: Logic) extends Logic
    case class Plus(lhs: Logic, rhs: Logic) extends Logic
    case class Minus(lhs: Logic, rhs: Logic) extends Logic

    case class Not(expr: Logic) extends Logic
    case class And(exprs: Seq[Logic]) extends Logic
    case class Or(exprs: Seq[Logic]) extends Logic
    case class Implie(lhs: Logic, rhs: Logic) extends Logic
  }
}
