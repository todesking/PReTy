package com.todesking.prety.universe

trait Graphs { self: Values with Preds with Constraints with UnknownPreds =>
  class Graph(
    val constraints: Seq[Constraint],
    val binding: Map[Value, Pred],
    envStack: List[PredEnv],
    currentEnv: PredEnv) {

    def pushEnv(): Graph =
      new Graph(constraints, binding, currentEnv :: envStack, currentEnv)
    def popEnv(): Graph =
      new Graph(constraints, binding, envStack.tail, envStack.head)
    def env(v: Value): Graph =
      new Graph(constraints, binding, envStack, currentEnv.add(v))

    def subtype(l: Value, r: UnknownPred): Graph =
      new Graph(constraints :+ Constraint.FocusLeft(currentEnv, l, r), binding, envStack, currentEnv)

    def +(rhs: Graph) = {
      val conflicts = this.binding.keySet intersect rhs.binding.keySet
      if (conflicts.nonEmpty) {
        throw new RuntimeException(s"Binding conflict: ${conflicts.mkString(", ")}")
      }
      new Graph(
        constraints ++ rhs.constraints,
        binding ++ rhs.binding,
        envStack,
        currentEnv)
    }

    def bind(vps: Map[Value, Pred]) =
      this + Graph.bind(vps)

    lazy val allValues = constraints.flatMap(_.values).toSet
    lazy val assignedValues = binding.keySet
    lazy val unassignedValues = allValues -- assignedValues

    // TODO: check unbound values
    def groundConstraints: Seq[GroundConstraint] =
      constraints.map(_.ground(binding))

    def hasUnassignedIncomingEdge(v: Value): Boolean =
      incomingEdges(v).flatMap(_.lhs.toValue).exists(unassignedValues)

    def incomingEdges(v: Value): Set[Constraint] =
      constraints.filter { c => c.rhs.toValue.contains(v) }.toSet

    @scala.annotation.tailrec
    final def infer(): Graph = {
      val next = infer0()
      if (next.binding == this.binding) this
      else next.infer()
    }

    private[this] final def infer0(): Graph = {
      // TODO: weaken with visibility
      val newBinding =
        unassignedValues
          .filterNot(hasUnassignedIncomingEdge)
          .foldLeft(binding) { (b, v) =>
            val p = Pred.and(incomingEdges(v).map(_.lhs).map(_.reveal(b)).toSeq)
            b + (v -> p)
          }
      new Graph(constraints, newBinding, envStack, currentEnv)
    }
  }
  object Graph {
    val empty: Graph = new Graph(Seq(), Map(), Nil, new PredEnv(Set()))
    def bind(v: Value, p: Pred): Graph =
      new Graph(Seq(), Map(v -> p), Nil, new PredEnv(Set()))
    def bind(vps: Map[Value, Pred]): Graph =
      new Graph(Seq(), vps, Nil, new PredEnv(Set()))
  }
}
