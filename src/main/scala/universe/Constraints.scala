package com.todesking.prety.universe

trait Constraints { self: Values with Preds =>
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

  case class GroundConstraint(constraint: Constraint, lhs: Pred, rhs: Pred) {
    override def toString = constraint match {
      case Constraint.FocusLeft(l, r) =>
        s"$lhs *<= $rhs"
      case Constraint.FocusRight(l, r) =>
        s"$lhs <=* $rhs"
    }
    def focus: Value = constraint.focus
  }
}
