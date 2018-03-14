package com.todesking.prety.universe

trait Values { self: ForeignTypes with Queries with Constraints with UnknownPreds =>
  sealed abstract class Value {
    val name: String
    val tpe: TypeSym
    def naked: Value.Naked
  }
  object Value {
    import scala.language.implicitConversions
    implicit def valueToUnknownPred(v: Value): UnknownPred.OfValue =
      UnknownPred.OfValue(v)

    sealed abstract class Naked extends Value

    case class Origin(id: Int, name: String, tpe: TypeSym) extends Naked {
      override def toString = s"$name#$id: $tpe"
      override def naked = this
    }
    case class IntLiteral(v: Int) extends Naked {
      override val name = s"int($v)"
      override val tpe = query.types.int
      override def naked = this
      override val toString = name
    }
    case class Ref(id: Int, parent: Value) extends Value {
      override val name = s"ref($parent)#$id"
      override val tpe = parent.tpe
      override def naked = parent.naked
      override val toString = name
    }
  }
  case class FunctionValue(self: Value, ret: Value, paramss: Seq[Seq[(String, Value)]])
}
