package com.todesking.prety

sealed abstract class Logic {
  def ===(rhs: Logic) = Logic.Eq(this, rhs)
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

  case class Var(id: Int, tpe: Type) extends Logic {
    override def toString = {
      val t = tpe match {
        case TInt => "i"
        case TBool => "b"
        case TString => "s"
      }
      s"${t}_$id"
    }
  }

  case class IntValue(value: Int) extends Logic {
    override def toString = value.toString
  }
  case class BoolValue(value: Boolean) extends Logic {
    override def toString = value.toString
  }
  case class StringValue(value: String) extends Logic

  val True = BoolValue(true)
  val False = BoolValue(false)

  case class App(fun: String, args: Seq[Logic]) extends Logic

  case class Eq(lhs: Logic, rhs: Logic) extends Logic {
    override def toString = s"$lhs == $rhs"
  }
  case class Lt(lhs: Logic, rhs: Logic) extends Logic
  case class Le(lhs: Logic, rhs: Logic) extends Logic
  case class Gt(lhs: Logic, rhs: Logic) extends Logic {
    override def toString = s"$lhs > $rhs"
  }
  case class Ge(lhs: Logic, rhs: Logic) extends Logic

  case class Neg(expr: Logic) extends Logic
  case class Plus(lhs: Logic, rhs: Logic) extends Logic
  case class Minus(lhs: Logic, rhs: Logic) extends Logic

  case class Not(expr: Logic) extends Logic
  case class And(exprs: Seq[Logic]) extends Logic
  case class Or(exprs: Seq[Logic]) extends Logic
  case class Implie(lhs: Logic, rhs: Logic) extends Logic {
    override def toString = s"{ $lhs }-->{ $rhs }"
  }
}
