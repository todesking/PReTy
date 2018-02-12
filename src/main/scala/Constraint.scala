package com.todesking.prety

// represents lhs <= rhs
sealed abstract class Constraint {
  def lhs: PredHolder
  def rhs: PredHolder

  def values: Set[Value] =
    lhs.toValue.toSet ++ rhs.toValue
  def focus: Value

  def ground(binding: Map[Value, Pred]): GroundConstraint =
    GroundConstraint(this, lhs.pred(binding), rhs.pred(binding))
}
object Constraint {
  // represents lhs <= rhs
  sealed abstract class LE extends Constraint
  case class FocusLeft(lhs: PredHolder.Variable, rhs: PredHolder) extends LE {
    override def toString = s"$lhs *<= $rhs"
    override def focus = lhs.value
  }
  case class FocusRight(lhs: PredHolder, rhs: PredHolder.Variable) extends LE {
    override def toString = s"$lhs <=* $rhs"
    override def focus = rhs.value
  }
}
