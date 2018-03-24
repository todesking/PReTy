package com.todesking.prety.universe

trait Graphs { self: Values with Preds with Constraints with Envs with Debugging =>
  case class Graph(
    constraints: Seq[Constraint],
    binding: Map[Value, Pred],
    envStack: List[Env],
    currentEnv: Env) {

    def let(name: String, value: Value): Graph =
      copy(currentEnv = currentEnv.bind(name -> value))

    def visible(v: Value*): Graph =
      copy(currentEnv = currentEnv.bindUnnamed(v: _*))

    def cond(v: Value): Graph =
      copy(currentEnv = currentEnv.cond(v))
    def condNot(v: Value): Graph =
      copy(currentEnv = currentEnv.uncond(v))

    def pushEnv(): Graph =
      copy(envStack = currentEnv :: envStack)
    def popEnv(): Graph =
      copy(envStack = envStack.tail, currentEnv = envStack.head.copy(unnamed = envStack.head.unnamed ++ currentEnv.unnamed))

    def subtype(l: Value, r: UnknownPred): Graph =
      copy(constraints = constraints :+ Constraint.FocusLeft(currentEnv, l, r))

    def subtypeR(l: UnknownPred, r: Value): Graph =
      copy(constraints = constraints :+ Constraint.FocusRight(currentEnv, l, r))

    def bind(vps: Map[Value, Pred]) =
      copy(binding = binding ++ vps)

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

    private final def infer0(): Graph = {
      // TODO: weaken with visibility
      val newBinding =
        unassignedValues
          .filterNot(hasUnassignedIncomingEdge)
          .foldLeft(binding) { (b, v) =>
            val p = Pred.and(incomingEdges(v).map(_.lhs).map(_.reveal(b)).toSeq)
            dprint(s"INFER $v: $p")
            b + (v -> p)
          }
      copy(binding = newBinding)
    }
  }
  object Graph {
    def build(env: Env): Graph = new Graph(Seq(), Map(), Nil, env)
  }
}
