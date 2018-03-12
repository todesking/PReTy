package com.todesking.prety.universe

trait ValueRepos { self: ForeignTypes with Queries with Values =>
  class ValueRepo {
    private[this] var nextValueId = 0
    private[this] def fresh(name: String, pos: Pos, tpe: TypeSym): Value = {
      val v = Value(nextValueId, name, tpe)
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

  val valueRepo = new ValueRepo
}
