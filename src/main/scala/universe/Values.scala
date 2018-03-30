package com.todesking.prety.universe

trait Values { self: ForeignTypes with Constraints with Preds =>
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
    case class BooleanLiteral(v: Boolean) extends Naked {
      override val name = s"boolean($v)"
      override val tpe = query.types.boolean
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

  class ValueRepo {
    private[this] var nextValueId = 0
    private[this] def fresh(name: String, pos: Pos, tpe: TypeSym): Value = {
      val v = Value.Origin(nextValueId, name, tpe)
      nextValueId += 1
      if (pos != query.emptyPos)
        positions = positions + (v -> pos)
      v
    }

    def newRef(origin: Value, pos: Pos): Value = {
      val v = Value.Ref(nextValueId, origin)
      nextValueId += 1
      if (pos != query.emptyPos)
        positions = positions + (v -> pos)
      v
    }

    private[this] var positions = Map.empty[Value, Pos]

    def getPos(v: Value): Option[Pos] =
      positions.get(v)
    // TODO: move pos to Value

    def setPos(v: Value, p: Pos): Unit =
      if (positions.get(v).map(query.samePos(_, p)) getOrElse false) throw new RuntimeException(s"Pos registered twice")
      else positions = positions + (v -> p)

    def newExpr(name: String, pos: Pos, tpe: TypeSym): Value =
      fresh(name, pos, tpe)

    private[this] var functionValues = Map.empty[DefSym, FunctionValue]
    def functionValue(f: DefSym): FunctionValue =
      functionValues.get(f) getOrElse {
        val name = query.name(f)
        val self = fresh(s"$name/this", query.pos(f), query.thisType(f))
        val v = FunctionValue(
          self,
          fresh(s"$name/ret", query.pos(f), query.returnType(f)),
          query.paramss(f).map(_.map(p =>
            query.name(p) -> localValue(p, self, name).ret)))
        functionValues = functionValues + (f -> v)
        v
      }
    private[this] def localValue(f: DefSym, self: Value, funName: String): FunctionValue =
      functionValues.get(f) getOrElse {
        val name = s"$funName/(${query.name(f)})"
        val v = FunctionValue(
          self,
          fresh(name, query.pos(f), query.returnType(f)),
          query.paramss(f).map(_.map(p =>
            query.name(f) -> localValue(p, self, name).ret)))
        functionValues = functionValues + (f -> v)
        v
      }
  }
}
