package com.todesking.prety.universe

trait Values { self: ForeignTypes with Preds with Constraints =>
  case class Value(id: Int, name: String, tpe: TypeSym) {
    override def toString = s"$name#$id: $tpe"
    def *<:=(rhs: PredHolder): Constraint = Constraint.FocusLeft(this, rhs)
  }
  object Value {
    import scala.language.implicitConversions
    implicit def valueToPredHolder(v: Value): PredHolder.Variable =
      PredHolder.Variable(v)
  }
}
