package com.todesking.prety

sealed abstract class Logic {
  // TODO: rename: freeVars
  def vars: Set[Logic.Var]

}
object Logic {
  def and(ls: Seq[Logic.LBool]): Logic = ls match {
    case Seq() => True
    case Seq(l) => l
    case many => And(many)
  }

  sealed abstract class LBool extends Logic {
  def unary_! = Logic.Not(this)
  def &(rhs: LBool) = rhs match {
    case True => this
    case x => And(Seq(this, x))
  }
  def |(rhs: LBool) = Or(Seq(this, rhs))
  def -->(rhs: LBool) = Implie(this, rhs)

  def universalQuantifiedForm: Logic =
    if (vars.isEmpty) this
    else Logic.Forall(this.vars, this)
  }
  sealed abstract class LInt extends Logic {
    def ===(rhs: LInt) = IEq(this, rhs)
    def <=(rhs: LInt) = Le(this, rhs)
    def <(rhs: LInt) = Lt(this, rhs)
    def >=(rhs: LInt) = Ge(this, rhs)
    def >(rhs: LInt) = Gt(this, rhs)

    def +(rhs: LInt) = Plus(this, rhs)
    def -(rhs: LInt) = Minus(this, rhs)
    def unary_- = Neg(this)
  }

  sealed trait Leaf extends Logic {
    override def vars = Set()
  }
  sealed trait BinOp[A <: Logic] extends Logic {
    val lhs: A
    val rhs: A
    override def vars = lhs.vars ++ rhs.vars
  }
  sealed abstract class ManyOp[A <: Logic] extends Logic {
    val items: Seq[A]
    override def vars = items.flatMap(_.vars).toSet
  }

  sealed trait Var extends Logic with Leaf {
    def id: Int
    def name: String
  }

  case class IVar(id: Int, name: String) extends LInt with Var {
    def varName = s"i_$id"
    override def toString = s"$varName($name)"
    override def vars = Set(this)
  }
  case class BVar(id: Int, name: String) extends LBool with Var {
    def varName = s"b_$id"
    override def toString = s"$varName($name)"
    override def vars = Set(this)
  }

  case class IValue(value: Int) extends LInt with Leaf {
    override def toString = value.toString
  }
  case class BValue(value: Boolean) extends LBool with Leaf {
    override def toString = value.toString
    override def &(rhs: LBool) =
      if (value) rhs
      else this
  }
  case class StringValue(value: String) extends Leaf

  val True = BValue(true)
  val False = BValue(false)

  // TODO: INT_*
  case class IEq(lhs: LInt, rhs: LInt) extends BinOp[LInt] {
    override def toString = s"$lhs == $rhs"
  }
  case class BEq(lhs: LBool, rhs: LBool) extends BinOp[LBool] {
    override def toString = s"$lhs == $rhs"
  }
  case class Lt(lhs: LInt, rhs: LInt) extends BinOp[LInt] {
    override def toString = s"$lhs < $rhs"
  }
  case class Le(lhs: LInt, rhs: LInt) extends BinOp[LInt]
  case class Gt(lhs: LInt, rhs: LInt) extends BinOp[LInt] {
    override def toString = s"$lhs > $rhs"
  }
  case class Ge(lhs: LInt, rhs: LInt) extends BinOp[LInt]

  case class Neg(expr: LInt) extends LInt {
    override def vars = expr.vars
  }
  case class Plus(lhs: LInt, rhs: LInt) extends BinOp[LInt]
  case class Minus(lhs: LInt, rhs: LInt) extends BinOp[LInt]

  case class Not(expr: LBool) extends LBool {
    override def vars = expr.vars
  }
  case class And(items: Seq[LBool]) extends ManyOp[LBool] {
    override def toString = items.mkString("(", " & ", ")")
  }
  case class Or(items: Seq[LBool]) extends ManyOp[LBool] {
    override def toString = items.mkString("(", " | ", ")")
  }
  case class Implie(lhs: LBool, rhs: LBool) extends BinOp[LBool] {
    override def toString = s"{ $lhs }-->{ $rhs }"
  }
  case class Forall(params: Set[Logic.Var], expr: LBool) extends LBool {
    override def vars = expr.vars -- params
    override def toString = s"forall ${params.toSeq.map(_.toString).sorted.mkString(", ")}. $expr"
  }
}
