package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Constraints { self: ForeignTypes with ForeignTypeOps with Values with UnknownPreds with Preds =>
  case class PredEnv(values: Set[Value]) {
    // TODO: add path condition
    def add(v: Value): PredEnv =
      PredEnv(values + v)
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

  // TODO: s/Ground/Concrete
  case class GroundConstraint(constraint: Constraint, lhs: Pred, rhs: Pred) {
    require(lhs.tpe <:< rhs.tpe)
    override def toString = constraint match {
      case Constraint.FocusLeft(e, l, r) =>
        s"$lhs *<= $rhs"
      case Constraint.FocusRight(e, l, r) =>
        s"$lhs <=* $rhs"
    }
    def focus: Value = constraint.focus
    def env = constraint.env
  }

  case class LogicConstraint(constraint: GroundConstraint, logic: Logic) {
    override def toString = s"$constraint; $logic"
  }
}
