package com.todesking.prety.universe

trait ValueRepos { self: ForeignTypes with Queries with Values =>
  class ValueRepo {
    private[this] var values = Map.empty[DefSym, Value]
    private[this] var thisValues = Map.empty[DefSym, Value]
    private[this] var positions = Map.empty[Value, Pos]
    private[this] var nextValueId = 0

    private[this] def register(key: DefSym, name: String, tpe: TypeSym): Value = {
      if (values.contains(key))
        throw new RuntimeException(s"Value conflict: $key")
      val v = fresh(name, query.emptyPos, tpe)
      values = values + (key -> v)
      v
    }

    private[this] def fresh(name: String, pos: Pos, tpe: TypeSym): Value = {
      val v = Value(nextValueId, name, tpe)
      nextValueId += 1
      if (pos != query.emptyPos)
        positions = positions + (v -> pos)
      v
    }

    def getPos(v: Value): Option[Pos] =
      positions.get(v)

    def setPos(v: Value, p: Pos): Unit =
      if (positions.get(v).map(query.samePos(_, p)) getOrElse false) throw new RuntimeException(s"Pos registered twice")
      else positions = positions + (v -> p)

    def newExpr(name: String, pos: Pos, tpe: TypeSym): Value =
      fresh(name, pos, tpe)

    def registerParam(fun: DefSym, p: DefSym): Value =
      register(p, s"${query.name(fun)}/(${query.name(p)})", query.returnType(p))

    def getOrRegisterReturn(fun: DefSym): Value =
      values.get(fun).getOrElse {
        register(fun, s"${query.name(fun)}/return", query.returnType(fun))
      }

    def getOrRegisterThis(fun: DefSym): Value =
      thisValues.get(fun).getOrElse {
        val v = fresh(s"${query.name(fun)}/this", query.emptyPos, query.thisType(fun))
        thisValues = thisValues + (fun -> v)
        v
      }
  }

  val valueRepo = new ValueRepo
}
