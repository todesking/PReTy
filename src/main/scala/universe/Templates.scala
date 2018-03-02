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
      aSelf: Value,
      aRet: Value,
      aArgss: Seq[Seq[Value]]): Graph = {
      require(argss.map(_.size) == aArgss.map(_.size))

      val argSub = Map(self -> aSelf) ++
        argss.flatten.zip(aArgss.flatten).map { case (p, a) => p -> a }

      Graph
        .constraint(aSelf *<:= self)
        .constraint(
          argss.flatten.zip(aArgss.flatten).map {
            case (p, a) =>
              a *<:= p.substitute(argSub)
          })
        .constraint(aRet *<:= ret.substitute(argSub))
    }
  }
}
