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
  def &(rhs: Logic) = rhs match {
    case Logic.True => this
    case x => Logic.And(Seq(this, x))
  }
  def |(rhs: Logic) = Logic.Or(Seq(this, rhs))
  def -->(rhs: Logic) = Logic.Implie(this, rhs)

  // TODO: rename: freeVars
  def vars: Set[Logic.Var]

  def universalQuantifiedForm: Logic =
    if (vars.isEmpty) this
    else Logic.Forall(this.vars, this)
}
object Logic {
  // TODO: claeess per type
  sealed abstract class Type
  case object TInt extends Type
  case object TBool extends Type

  def and(ls: Seq[Logic]): Logic = ls match {
    case Seq() => True
    case Seq(l) => l
    case many => And(many)
  }

  sealed abstract class Leaf extends Logic {
    override def vars = Set()
  }
  sealed abstract class BinOp extends Logic {
    val lhs: Logic
    val rhs: Logic
    override def vars = lhs.vars ++ rhs.vars
  }
  sealed abstract class ManyOp extends Logic {
    val items: Seq[Logic]
    override def vars = items.flatMap(_.vars).toSet
  }

  case class Var(id: Int, tpe: Type, name: String) extends Leaf {
    def varName = {
      val t = tpe match {
        case TInt => "i"
        case TBool => "b"
      }
      s"${t}_$id"
    }
    override def toString = s"$varName($name)"
    override def vars = Set(this)
  }

  case class IntValue(value: Int) extends Leaf {
    override def toString = value.toString
  }
  case class BoolValue(value: Boolean) extends Leaf {
    override def toString = value.toString
    override def &(rhs: Logic) =
      if (value) rhs
      else this

    def unapply(l: Logic): Option[BoolValue] = l match {
      case BoolValue(v) if v == value =>
        Some(this)
      case _ =>
        None
    }
  }
  case class StringValue(value: String) extends Leaf

  val True = BoolValue(true)
  val False = BoolValue(false)

  // TODO: INT_*
  case class Eq(lhs: Logic, rhs: Logic) extends BinOp {
    override def toString = s"$lhs == $rhs"
  }
  case class Lt(lhs: Logic, rhs: Logic) extends BinOp {
    override def toString = s"$lhs < $rhs"
  }
  case class Le(lhs: Logic, rhs: Logic) extends BinOp
  case class Gt(lhs: Logic, rhs: Logic) extends BinOp {
    override def toString = s"$lhs > $rhs"
  }
  case class Ge(lhs: Logic, rhs: Logic) extends BinOp

  case class Neg(expr: Logic) extends Logic {
    override def vars = expr.vars
  }
  case class Plus(lhs: Logic, rhs: Logic) extends BinOp
  case class Minus(lhs: Logic, rhs: Logic) extends BinOp

  case class Not(expr: Logic) extends Logic {
    override def vars = expr.vars
  }
  case class And(items: Seq[Logic]) extends ManyOp {
    override def toString = items.mkString("(", " & ", ")")
  }
  case class Or(items: Seq[Logic]) extends ManyOp {
    override def toString = items.mkString("(", " | ", ")")
  }
  case class Implie(lhs: Logic, rhs: Logic) extends BinOp {
    override def toString = s"{ $lhs }-->{ $rhs }"
  }
  case class Forall(params: Set[Logic.Var], expr: Logic) extends Logic {
    override def vars = expr.vars -- params
    override def toString = s"forall ${params.toSeq.map(_.toString).sorted.mkString(", ")}. $expr"
  }
}
