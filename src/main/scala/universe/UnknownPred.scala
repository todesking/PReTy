package com.todesking.prety.universe

trait UnknownPreds { self: Values with Preds =>
  sealed abstract class UnknownPred {
    def reveal(binding: Map[Value, Pred]): Pred
    def toValue: Option[Value]
  }
  object UnknownPred {
    case class OfValue(value: Value) extends UnknownPred {
      def substitute(mapping: Map[Value, Value]) =
        Substitute(mapping, this)
      override def reveal(binding: Map[Value, Pred]) = binding(value)
      override def toValue = Some(value)
      override def toString = s"?($value)"
    }

    case class Substitute(mapping: Map[Value, Value], original: UnknownPred) extends UnknownPred {
      override def reveal(binding: Map[Value, Pred]) =
        original.reveal(binding).substitute(mapping)
      override def toValue = original.toValue
      override def toString = s"[${mapping.toSeq.map { case (k, v) => s"$k -> $v" }.mkString(", ")}](${original.toValue.get})"
    }
  }
}
