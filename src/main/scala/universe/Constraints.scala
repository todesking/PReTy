package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Constraints { self: ForeignTypes with ForeignTypeOps with Values with UnknownPreds with Preds with Envs =>
  // represents lhs <= rhs
  sealed abstract class Constraint {
    def env: Env
    def lhs: UnknownPred
    def rhs: UnknownPred

    def values: Set[Value] =
      lhs.toValue.toSet ++ rhs.toValue
    def focus: Value

    // TODO: tpe

    def ground(binding: Map[Value, Pred]): GroundConstraint =
      GroundConstraint(this, lhs.reveal(binding), rhs.reveal(binding))
  }
  object Constraint {
    // represents lhs <= rhs
    sealed abstract class LE extends Constraint
    case class FocusLeft(env: Env, lhs: UnknownPred.OfValue, rhs: UnknownPred) extends LE {
      override def toString = s"$lhs *<= $rhs"
      override def focus = lhs.value
    }
    case class FocusRight(env: Env, lhs: UnknownPred, rhs: UnknownPred.OfValue) extends LE {
      override def toString = s"$lhs <=* $rhs"
      override def focus = rhs.value
    }
  }

  // TODO: s/Ground/Concrete
  case class GroundConstraint(constraint: Constraint, lhs: Pred, rhs: Pred) {
    override def toString = constraint match {
      case Constraint.FocusLeft(e, l, r) =>
        s"$lhs *<= $rhs"
      case Constraint.FocusRight(e, l, r) =>
        s"$lhs <=* $rhs"
    }
    def focus: Value = constraint.focus
    def env = constraint.env
    def messageString = s"${lhs.messageString} *<= ${rhs.messageString}"
  }

  case class LogicConstraint(constraint: GroundConstraint, logic: Logic) {
    override def toString = s"(${constraint.env.values.mkString(", ")})(${constraint.env.conds.mkString(", ")})(${constraint.env.unconds.mkString(",")}) => $constraint; $logic"
  }
}
