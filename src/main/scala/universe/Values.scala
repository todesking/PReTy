package com.todesking.prety.universe

trait Values { self: Preds with Constraints =>
case class Value(id: Int, name: String) {
  override def toString = s"$name#$id"
  def *<:=(rhs: PredHolder): Constraint = Constraint.FocusLeft(this, rhs)
}
object Value {
  import scala.language.implicitConversions
  implicit def valueToPredHolder(v: Value): PredHolder.Variable =
    PredHolder.Variable(v)
}
}
