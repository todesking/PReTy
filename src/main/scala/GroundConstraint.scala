package com.todesking.prety

case class GroundConstraint(constraint: Constraint, lhs: Pred, rhs: Pred) {
  override def toString = constraint match {
    case Constraint.FocusLeft(l, r) =>
      s"$lhs *<= $rhs"
    case Constraint.FocusRight(l, r) =>
      s"$lhs <=* $rhs"
  }
  def focus: Value = constraint.focus
}

