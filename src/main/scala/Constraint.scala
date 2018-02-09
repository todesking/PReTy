package com.todesking.prety

// represents lhs <= rhs
sealed abstract class Constraint {
  def lhs: PredHolder
  def rhs: PredHolder

  def values: Set[Value] =
    lhs.toValue.toSet ++ rhs.toValue
}
object Constraint {
  // represents lhs <= rhs
  sealed abstract class LE extends Constraint
  case class FocusLeft(lhs: PredHolder.Variable, rhs: PredHolder) extends LE {
    override def toString = s"$lhs *<= $rhs"
  }
  case class FocusRight(lhs: PredHolder, rhs: PredHolder.Variable) extends LE {
    override def toString = s"$lhs <=* $rhs"
  }
}
