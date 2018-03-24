package com.todesking.prety

sealed abstract class Logic {
  // TODO: rename: freeVars
  def vars: Set[Logic.Var]
  def substitute(f: PartialFunction[Logic, Logic]): Logic
  def substituteChild(f: PartialFunction[Logic, Logic]): Logic
}
object Logic {
  def and(ls: Iterable[Logic.LBool]): Logic.LBool = ls match {
    case Seq() => True
    case Seq(l) => l
    case many => many.reduce(_ & _)
  }

  sealed abstract class LBool extends Logic {
    def ===(rhs: LBool): LBool = BEq(this, rhs)
    def unary_!(): LBool = Logic.Not(this)
    def &(rhs: LBool): LBool = rhs match {
      case True => this
      case And(xs) => And(this +: xs)
      case x => And(Seq(this, x))
    }
    def |(rhs: LBool): LBool = Or(Seq(this, rhs))
    def -->(rhs: LBool): LBool = Implie(this, rhs)
    def <->(rhs: LBool): LBool = BEq(this, rhs)

    def universalQuantifiedForm: Logic =
      if (vars.isEmpty) this
      else Logic.Forall(this.vars, this)
    override def substitute(f: PartialFunction[Logic, Logic]): LBool = f.lift(this).map(_.asInstanceOf[LBool]) getOrElse substituteChild(f)
    override def substituteChild(f: PartialFunction[Logic, Logic]): LBool
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
    override def substitute(f: PartialFunction[Logic, Logic]): LInt = f.lift(this).map(_.asInstanceOf[LInt]) getOrElse substituteChild(f)
    override def substituteChild(f: PartialFunction[Logic, Logic]): LInt
  }

  sealed trait Var extends Logic {
    def id: Int
    def name: String
    def varName: String
    override def vars = Set(this)
    override def toString = s"$varName($name)"
  }

  case class IVar(id: Int, name: String) extends LInt with Var {
    override def varName = s"i_$id"
    override def substituteChild(f: PartialFunction[Logic, Logic]) = this
  }
  case class BVar(id: Int, name: String) extends LBool with Var {
    override def varName = s"b_$id"
    override def substituteChild(f: PartialFunction[Logic, Logic]) = this
  }

  sealed abstract class ILeaf extends LInt {
    override def vars = Set()
    override def substituteChild(f: PartialFunction[Logic, Logic]) = this
  }
  sealed abstract class BLeaf extends LBool {
    override def vars = Set()
    override def substituteChild(f: PartialFunction[Logic, Logic]) = this
  }

  case class IValue(value: Int) extends ILeaf {
    override def toString = value.toString
  }
  case class BValue(value: Boolean) extends BLeaf {
    override def toString = value.toString
    override def &(rhs: LBool) =
      if (value) rhs
      else this
  }

  val True = BValue(true)
  val False = BValue(false)

  sealed abstract class IBin extends LInt {
    def lhs: LInt
    def rhs: LInt
    override def vars = lhs.vars ++ rhs.vars
  }
  sealed abstract class BBin[A <: Logic] extends LBool {
    def lhs: A
    def rhs: A
    override def vars = lhs.vars ++ rhs.vars
  }
  sealed trait BinOp[A <: Logic] extends Logic {
    def lhs: A
    def rhs: A
    override def vars = lhs.vars ++ rhs.vars
  }
  sealed trait ManyOp[A <: Logic, B <: Logic] extends Logic {
    def items: Seq[B]
    override def vars = items.flatMap(_.vars).toSet
  }

  case class IEq(lhs: LInt, rhs: LInt) extends BBin[LInt] {
    override def toString = s"$lhs == $rhs"
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      IEq(lhs.substitute(f), rhs.substitute(f))
  }
  case class BEq(lhs: LBool, rhs: LBool) extends BBin[LBool] {
    override def toString = s"$lhs == $rhs"
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      BEq(lhs.substitute(f), rhs.substitute(f))
  }
  case class Lt(lhs: LInt, rhs: LInt) extends BBin[LInt] {
    override def toString = s"$lhs < $rhs"
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Lt(lhs.substitute(f), rhs.substitute(f))
  }
  case class Le(lhs: LInt, rhs: LInt) extends BBin[LInt] {
    override def toString = s"$lhs <= $rhs"
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Le(lhs.substitute(f), rhs.substitute(f))
  }
  case class Gt(lhs: LInt, rhs: LInt) extends BBin[LInt] {
    override def toString = s"$lhs > $rhs"
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Gt(lhs.substitute(f), rhs.substitute(f))
  }
  case class Ge(lhs: LInt, rhs: LInt) extends BBin[LInt] {
    override def toString = s"$lhs >= $rhs"
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Ge(lhs.substitute(f), rhs.substitute(f))
  }

  case class Neg(expr: LInt) extends LInt {
    override def vars = expr.vars
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Neg(expr.substitute(f))
  }
  case class Plus(lhs: LInt, rhs: LInt) extends IBin {
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Plus(lhs.substitute(f), rhs.substitute(f))
  }
  case class Minus(lhs: LInt, rhs: LInt) extends IBin {
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Minus(lhs.substitute(f), rhs.substitute(f))
  }
  case class Div(lhs: LInt, rhs: LInt) extends IBin {
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Div(lhs.substitute(f), rhs.substitute(f))
  }
  case class Mul(lhs: LInt, rhs: LInt) extends IBin {
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Mul(lhs.substitute(f), rhs.substitute(f))
  }

  case class Not(expr: LBool) extends LBool {
    override def vars = expr.vars
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Not(expr.substitute(f))
  }
  case class And(items: Seq[LBool]) extends LBool with ManyOp[LBool, LBool] {
    override def &(rhs: LBool) = rhs match {
      case And(xs) => And(items ++ xs)
      case True => this
      case x => And(items :+ x)
    }
    override def toString = items.map(i => s"($i)").mkString(" & ")
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      And(items.map(_.substitute(f)))
  }
  case class Or(items: Seq[LBool]) extends LBool with ManyOp[LBool, LBool] {
    override def toString = items.mkString("(", " | ", ")")
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Or(items.map(_.substitute(f)))
  }
  case class Implie(lhs: LBool, rhs: LBool) extends LBool with BinOp[LBool] {
    override def toString = s"{ $lhs }-->{ $rhs }"
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Implie(lhs.substitute(f), rhs.substitute(f))
  }
  case class Forall(params: Set[Logic.Var], expr: LBool) extends LBool {
    override def vars = expr.vars -- params
    override def toString = s"forall ${params.toSeq.map(_.toString).sorted.mkString(", ")}. $expr"
    // TODO: Weird semantics...
    override def substituteChild(f: PartialFunction[Logic, Logic]) =
      Forall(params, expr.substitute(f))
  }
}
