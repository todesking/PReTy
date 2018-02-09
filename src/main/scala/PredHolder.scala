package com.todesking.prety

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
