package com.todesking.prety.scalac_plugin.universe

trait Constraints { self: ASTs with Preds =>
  sealed abstract class PredHolder {
    def pred(binding: Map[Value, Pred]): Pred
    def toValue: Option[Value]
  }
  object PredHolder {
    case class Variable(value: Value) extends PredHolder {
      def substitute(mapping: Map[Value, Value]) =
        Substitute(mapping, this)
      override def pred(binding: Map[Value, Pred]) = binding(value)
      override def toValue = Some(value)
      override def toString = s"?($value)"
    }

    case class Substitute(mapping: Map[Value, Value], original: PredHolder) extends PredHolder {
      override def pred(binding: Map[Value, Pred]) =
        original.pred(binding).substitute(mapping)
      override def toValue = original.toValue
      override def toString = s"[${mapping.toSeq.map { case (k, v) => s"$k -> $v" }.mkString(", ")}](${original.toValue.get})"
    }
  }

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
}
