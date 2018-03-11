package com.todesking.prety.universe

trait Templates { self: Preds with Graphs with Values with UnknownPreds =>
  case class Template(
    self: Value,
    ret: Value,
    argss: Seq[Seq[(String, Value)]],
    bindings: Map[Value, Pred]) {
    override def toString =
      s"$self.(${argss.map(_.map(_._1).mkString("(", ", ", ")")).mkString("")}) = $ret"
    // TODO: check acyclic
    def apply(
      graph: Graph,
      aSelf: Value,
      aRet: Value,
      aArgss: Seq[Seq[Value]]): Graph = {
      require(argss.map(_.size) == aArgss.map(_.size))

      val argSub = Map(self -> aSelf) ++
        argss.flatten.zip(aArgss.flatten).map { case ((n, p), a) => p -> a }

      // TODO: tsort args
      argss.flatten.zip(aArgss.flatten).foldLeft {
        graph.pushEnv()
          .subtype(aSelf, self)
          .let("this", aSelf)
      } {
        case (g, ((name, p), a)) =>
          g.subtype(a, p.substitute(argSub)).let(name, a)
      }.alias(aRet, ret.substitute(argSub))
        .popEnv()
    }
  }
}
