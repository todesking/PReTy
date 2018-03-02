package com.todesking.prety.universe

trait Constraints { self: Values with UnknownPreds with Preds =>
  // represents lhs <= rhs
  sealed abstract class Constraint {
    def lhs: UnknownPred
    def rhs: UnknownPred

    def values: Set[Value] =
      lhs.toValue.toSet ++ rhs.toValue
    def focus: Value

    def ground(binding: Map[Value, Pred]): GroundConstraint =
      GroundConstraint(this, lhs.reveal(binding), rhs.reveal(binding))
  }
  object Constraint {
    // represents lhs <= rhs
    sealed abstract class LE extends Constraint
    case class FocusLeft(lhs: UnknownPred.OfValue, rhs: UnknownPred) extends LE {
      override def toString = s"$lhs *<= $rhs"
      override def focus = lhs.value
    }
    case class FocusRight(lhs: UnknownPred, rhs: UnknownPred.OfValue) extends LE {
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
