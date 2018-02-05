package com.todesking.prety.scalac_plugin.universe

trait Constraints { self: ASTs with Preds =>
  sealed abstract class PredHolder {
    def substitute(mapping: Map[Value, Value]): PredHolder
    def pred(binding: Map[Value, Pred]): Pred
    def toValue: Option[Value]
  }
  object PredHolder {
    case class Variable(value: Value) extends PredHolder {
      override def substitute(mapping: Map[Value, Value]) =
        Substitute(mapping, value)
      override def pred(binding: Map[Value, Pred]) = binding(value)
      override def toValue = Some(value)
      override def toString = s"?($value)"
    }

    case class Substitute(mapping: Map[Value, Value], value: Value) extends PredHolder {
      override def substitute(m: Map[Value, Value]) = ???
      override def pred(binding: Map[Value, Pred]) = ???
      override def toValue = Some(value)
      override def toString = s"[${mapping.toSeq.map { case (k, v) => s"$k -> $v" }.mkString(", ")}]($value)"
    }

    case class Ground(pred: Pred) extends PredHolder {
      override def substitute(mapping: Map[Value, Value]) =
        ???
      override def pred(binding: Map[Value, Pred]) = pred
      override def toValue = None
      override def toString = pred.toString
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
