package com.todesking.prety.universe

trait Templates { self: Preds with Graphs with Values with UnknownPreds =>
  case class Template(
    self: Value,
    ret: Value,
    argss: Seq[Seq[Value]],
    bindings: Map[Value, Pred]) {
    override def toString =
      s"$self.(${argss.map(_.map(_.toString).mkString("(", ", ", ")")).mkString("")}) = $ret"
    // TODO: check acyclic
    def apply(
      graph: Graph,
      aSelf: Value,
      aRet: Value,
      aArgss: Seq[Seq[Value]]): Graph = {
      require(argss.map(_.size) == aArgss.map(_.size))

      val argSub = Map(self -> aSelf) ++
        argss.flatten.zip(aArgss.flatten).map { case (p, a) => p -> a }

      // TODO: tsort args
      argss.flatten.zip(aArgss.flatten).foldLeft {
        graph.pushEnv
          .subtype(aSelf, self)
          .env(aSelf)
      } {
        case (g, (p, a)) =>
          g.subtype(a, p.substitute(argSub)).env(a)
      }.alias(aRet, ret.substitute(argSub))
        .popEnv
    }
  }
}
