package com.todesking.prety.universe

trait Values { self: ForeignTypes with Constraints with UnknownPreds =>
  case class Value(id: Int, name: String, tpe: TypeSym) {
    override def toString = s"$name#$id: $tpe"
    def *<:=(rhs: UnknownPred): Constraint = Constraint.FocusLeft(this, rhs)
  }
  object Value {
    import scala.language.implicitConversions
    implicit def valueToUnknownPred(v: Value): UnknownPred.OfValue =
      UnknownPred.OfValue(v)
  }
}
