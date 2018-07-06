package com.todesking.prety.universe

import com.todesking.prety.util.uniqueMap

trait Graphs { self: Values with Preds with Templates with Constraints with Envs with Worlds with Debugging =>
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

    def template(t: Template): Graph =
      bind(t.bindings)

    def subtype(l: Value, r: UnknownPred): Graph =
      copy(constraints = constraints :+ Constraint.FocusLeft(currentEnv, l, r))

    def subtypeR(l: UnknownPred, r: Value): Graph =
      copy(constraints = constraints :+ Constraint.FocusRight(currentEnv, l, r))

    // TODO: check overwrite
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
      incomingEdges(v).flatMap(_.lhs.dependencies.map(_.naked)).exists(unassignedValues)

    def incomingEdges(v: Value.Naked): Set[Constraint] =
      // TODO: it smells...
      constraints.filter { c => c.rhs.dependencies.map(_.naked).contains(v) }.toSet

    def infer(w: World): Graph = {
      val next = prepareBindings(w).infer0(w)
      if (next.binding == this.binding) this
      else next.infer(w)
    }

    private[this] def prepareBindings(w: World): Graph = {
      copy(
        binding = binding ++ unassignedValues.collect {
          case v @ Value.IntLiteral(i) =>
            v -> w.preds.exactInt(i)
          case v @ Value.BooleanLiteral(b) =>
            v -> w.preds.exactBoolean(b)
        })
    }

    protected def infer0(w: World): Graph = {
      val newBinding =
        unassignedValues
          .filterNot(hasUnassignedIncomingEdge)
          .foldLeft(binding) { (b, v) =>
            val incoming = incomingEdges(v)
            val preds = incoming.map(_.lhs).map(_.reveal(b)).toSeq
            val p = if (preds.isEmpty) w.defaultPred(v.tpe) else Pred.and(preds) // TODO: [BUG] Use OR, not AND
            dprint(s"INFER ${v.shortString}: $p")
            incoming.zip(preds).foreach {
              case (i, p) =>
                dprint("  <-", i.lhs, p)
            }
            b + (v -> p)
          }
      copy(binding = newBinding)
    }
  }
  object Graph {
    def build(env: Env): Graph = new Graph(Seq(), Map(), Nil, env)
  }
}
