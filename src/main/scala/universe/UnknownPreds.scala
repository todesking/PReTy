package com.todesking.prety.universe

trait UnknownPreds { self: Values with Preds =>
  sealed abstract class UnknownPred {
    def reveal(binding: Map[Value, Pred]): Pred = revealOpt(binding) getOrElse {
      throw new RuntimeException(s"Can't reveal $this(env=$binding)")
    }
    def revealOpt(binding: Map[Value, Pred]): Option[Pred]
    def toValue: Option[Value]
  }
  object UnknownPred {
    case class OfValue(value: Value) extends UnknownPred {
      def substitute(mapping: Map[Value, Value]) =
        Substitute(mapping, this)
      override def revealOpt(binding: Map[Value, Pred]) = binding.get(value)
      override def toValue = Some(value)
      override def toString = s"?($value)"
    }

    case class Substitute(mapping: Map[Value, Value], original: UnknownPred) extends UnknownPred {
      override def revealOpt(binding: Map[Value, Pred]) =
        original.revealOpt(binding).map(_.substitute(mapping))
      override def toValue = original.toValue
      override def toString = s"[${mapping.toSeq.map { case (k, v) => s"$k -> $v" }.mkString(", ")}](${original.toValue.get})"
    }
  }
}
