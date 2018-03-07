package com.todesking.prety.universe

trait Constraints { self: ForeignTypes with ForeignTypeOps with Values with UnknownPreds with Preds =>
  class PredEnv {
  }

  // represents lhs <= rhs
  sealed abstract class Constraint {
    def env: PredEnv
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
    case class FocusLeft(env: PredEnv, lhs: UnknownPred.OfValue, rhs: UnknownPred) extends LE {
      override def toString = s"$lhs *<= $rhs"
      override def focus = lhs.value
    }
    case class FocusRight(env: PredEnv, lhs: UnknownPred, rhs: UnknownPred.OfValue) extends LE {
      override def toString = s"$lhs <=* $rhs"
      override def focus = rhs.value
    }
  }

  case class GroundConstraint(constraint: Constraint, lhs: Pred, rhs: Pred) {
    require(lhs.tpe <:< rhs.tpe)
    override def toString = constraint match {
      case Constraint.FocusLeft(e, l, r) =>
        s"$lhs *<= $rhs"
      case Constraint.FocusRight(e, l, r) =>
        s"$lhs <=* $rhs"
    }
    def focus: Value = constraint.focus
  }
}
