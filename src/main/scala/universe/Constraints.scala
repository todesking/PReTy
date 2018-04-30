package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Constraints { self: ForeignTypes with Values with Preds with Envs with Exprs with Props =>
  // represents lhs <= rhs
  sealed abstract class Constraint {
    def env: Env
    def lhs: UnknownPred
    def rhs: UnknownPred

    def values: Set[Value] =
      lhs.toValue.toSet ++ rhs.toValue
    def focus: Value

    // TODO: tpe

    def ground(binding: Map[Value.Naked, Pred]): GroundConstraint =
      GroundConstraint(this, binding, lhs.toValue, rhs.toValue, lhs.reveal(binding), rhs.reveal(binding))

    def arrowString: String
    override def toString = s"$lhs $arrowString $rhs"
  }
  object Constraint {
    // represents lhs <= rhs
    sealed abstract class LE extends Constraint
    case class FocusLeft(env: Env, lhs: UnknownPred.OfValue, rhs: UnknownPred) extends LE {
      override def arrowString = "*<="
      override def focus = lhs.value
    }
    case class FocusRight(env: Env, lhs: UnknownPred, rhs: UnknownPred.OfValue) extends LE {
      override def arrowString = "<=*"
      override def focus = rhs.value
    }
  }

  // TODO: s/Ground/Concrete
  case class GroundConstraint(constraint: Constraint, binding: Map[Value.Naked, Pred], lvalue: Option[Value], rvalue: Option[Value], lhs: Pred, rhs: Pred) {
    override def toString =
      s"${lvalue getOrElse "(no value)"} $lhs ${constraint.arrowString} $rhs ${rvalue getOrElse "(no value)"}"
    def focus: Value = constraint.focus
    def env = constraint.env
    def messageString = s"${lhs.messageString} ${constraint.arrowString} ${rhs.messageString}"
  }

  case class LogicConstraint(constraint: GroundConstraint, logic: Logic) {
    private[this] def valueString(v: Value) = s"$v: ${constraint.binding(v.naked)}"
    override def toString = s"(${constraint.env.values.map(valueString).mkString(", ")})(${constraint.env.conds.toSeq.map(valueString).mkString(", ")})(${constraint.env.unconds.map(valueString) mkString (",")}) => $constraint; $logic"
  }
}
