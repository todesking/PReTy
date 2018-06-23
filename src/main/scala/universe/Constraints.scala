package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Constraints { self: ForeignTypes with Values with Preds with Envs with Exprs with Props =>
  // represents lhs <= rhs
  sealed abstract class Constraint {
    def env: Env
    def lhs: UnknownPred
    def rhs: UnknownPred

    def values: Set[Value] =
      lhs.dependencies ++ rhs.dependencies
    def focus: Value

    // lhs <:< rhs under type tpe
    // TODO: It smells...
    // TODO: rhs.tpe
    def tpe: TypeSym = focus.tpe

    def ground(binding: Map[Value.Naked, Pred]): GroundConstraint =
      GroundConstraint(this, lhs.reveal(binding), rhs.reveal(binding))

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
  case class GroundConstraint(constraint: Constraint, lhs: Pred, rhs: Pred) {
    require(constraint.lhs.tpe <:< lhs.tpe, s"$constraint, ${constraint.lhs.tpe}, $lhs, ${lhs.tpe}")
    require(constraint.rhs.tpe <:< rhs.tpe, s"$constraint, ${constraint.rhs.tpe}, $rhs, ${rhs.tpe}")
    require(lhs.tpe <:< rhs.tpe)
    override def toString =
      s"$lhs ${constraint.arrowString} $rhs"
    def focus: Value = constraint.focus
    def env = constraint.env
    def messageString = s"${lhs.messageString} ${constraint.arrowString} ${rhs.messageString}"
    def tpe = constraint.tpe
  }

  case class LogicConstraint(constraint: GroundConstraint, logic: Logic) {
    private[this] def valueString(v: Value) = v.shortString
    override def toString = s"(${constraint.env.values.map(valueString).mkString(", ")})(${constraint.env.conds.toSeq.map(valueString).mkString(", ")})(${constraint.env.unconds.map(valueString) mkString (",")}) => $constraint; $logic"
  }
}
