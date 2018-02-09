package com.todesking.prety.universe

import com.todesking.prety.Value

trait ValueRepos { self: ForeignTypes with Queries =>
  class ValueRepo {
    private[this] var values = Map.empty[DefSym, Value]
    private[this] var thisValues = Map.empty[DefSym, Value]
    private[this] var positions = Map.empty[Value, Pos]
    private[this] var nextValueId = 0

    private[this] def register(key: DefSym, name: String): Value = {
      if (values.contains(key))
        throw new RuntimeException(s"Value conflict: $key")
      val v = fresh(name)
      values = values + (key -> v)
      v
    }

    def getPos(v: Value): Option[Pos] =
      positions.get(v)

    def fresh(name: String): Value = {
      val v = Value(nextValueId, name)
      nextValueId += 1
      v
    }
    def registerParam(fun: DefSym, p: DefSym): Value =
      register(p, s"${query.name(fun)}/(${query.name(p)})")

    def getOrRegisterReturn(fun: DefSym): Value =
      values.get(fun).getOrElse {
        register(fun, s"${query.name(fun)}/return")
      }

    def getOrRegisterThis(fun: DefSym): Value =
      thisValues.get(fun).getOrElse {
        val v = fresh(s"${query.name(fun)}/this")
        thisValues = thisValues + (fun -> v)
        v
      }
  }

  val valueRepo = new ValueRepo
}
