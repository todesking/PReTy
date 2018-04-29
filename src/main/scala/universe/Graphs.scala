package com.todesking.prety.universe

import com.todesking.prety.util.uniqueMap

trait Graphs { self: Values with Preds with Constraints with Envs with Debugging =>
  case class Graph(
    constraints: Seq[Constraint],
    binding: Map[Value.Naked, Pred],
    envStack: List[Env],
    currentEnv: Env) {

    def let(name: String, value: Value): Graph =
      copy(currentEnv = currentEnv.bind(name -> value))

    def cond(v: Value): Graph =
      copy(currentEnv = currentEnv.cond(v))
    def condNot(v: Value): Graph =
      copy(currentEnv = currentEnv.uncond(v))

    def pushEnv(): Graph =
      copy(envStack = currentEnv :: envStack)
    def popEnv(): Graph =
      copy(envStack = envStack.tail, currentEnv = envStack.head)

    def subtype(l: Value, r: UnknownPred): Graph =
      copy(constraints = constraints :+ Constraint.FocusLeft(currentEnv, l, r))

    def subtypeR(l: UnknownPred, r: Value): Graph =
      copy(constraints = constraints :+ Constraint.FocusRight(currentEnv, l, r))

    def bind(vps: Map[Value, Pred]): Graph =
      copy(binding = binding ++ vps.map { case (k, v) => k.naked -> v })

    def bind(vp: (Value, Pred)*): Graph =
      bind(uniqueMap(vp))

    lazy val allValues = constraints.flatMap(_.values.map(_.naked)).toSet
    lazy val assignedValues = binding.keySet
    lazy val unassignedValues = allValues -- assignedValues

    // TODO: check unbound values
    def groundConstraints: Seq[GroundConstraint] =
      constraints.map(_.ground(binding))

    def hasUnassignedIncomingEdge(v: Value.Naked): Boolean =
      incomingEdges(v).flatMap(_.lhs.toValue.map(_.naked)).exists(unassignedValues)

    def incomingEdges(v: Value.Naked): Set[Constraint] =
      constraints.filter { c => c.rhs.toValue.contains(v) }.toSet

    def infer(): Graph = {
      val next = prepareBindings().infer0()
      if (next.binding == this.binding) this
      else next.infer0()
    }

    private[this] def prepareBindings(): Graph = {
      copy(
        binding = binding ++ unassignedValues.collect {
          case v @ Value.IntLiteral(i) =>
            v -> Pred.exactInt(i)
          case v @ Value.BooleanLiteral(b) =>
            v -> Pred.exactBoolean(b)
        })
    }

    protected def infer0(): Graph = {
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
