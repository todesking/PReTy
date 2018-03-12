package com.todesking.prety.universe

trait Graphs { self: Values with Preds with Constraints with UnknownPreds with Envs =>
  case class Graph(
    val constraints: Seq[Constraint],
    val binding: Map[Value, Pred],
    val aliases: Map[Value, UnknownPred],
    envStack: List[Env],
    currentEnv: Env) {

    def let(name: String, value: Value): Graph =
      copy(currentEnv = currentEnv.bind(name -> value))

    def visible(v: Value*): Graph =
      copy(currentEnv = currentEnv.bindUnnamed(v: _*))

    def pushEnv(): Graph =
      copy(envStack = currentEnv :: envStack)
    def popEnv(): Graph =
      copy(envStack = envStack.tail, currentEnv = envStack.head)

    def subtype(l: Value, r: UnknownPred): Graph =
      copy(constraints = constraints :+ Constraint.FocusLeft(currentEnv, l, r))

    def bind(vps: Map[Value, Pred]) =
      copy(binding = binding ++ vps)

    def alias(from: Value, to: UnknownPred): Graph = {
      if (aliases.contains(from))
        throw new RuntimeException(s"Alias conflict: old=${aliases(from)}, new=${to}")
      copy(aliases = aliases + (from -> to))
    }

    lazy val allValues = constraints.flatMap(_.values).toSet ++ aliases.keys.toSet
    lazy val assignedValues = binding.keySet
    lazy val unassignedValues = allValues -- assignedValues
    lazy val unassignedAliases = aliases.keys.filter(!binding.contains(_))

    // TODO: check unbound values
    def groundConstraints: Seq[GroundConstraint] =
      constraints.map(_.ground(binding))

    def hasUnassignedIncomingEdge(v: Value): Boolean =
      incomingEdges(v).flatMap(_.lhs.toValue).exists(unassignedValues)

    def incomingEdges(v: Value): Set[Constraint] =
      constraints.filter { c => c.rhs.toValue.contains(v) }.toSet

    @scala.annotation.tailrec
    final def infer(): Graph = {
      val next = fillAlias().infer0()
      if (next.binding == this.binding) this
      else next.infer()
    }

    private def fillAlias(): Graph = {
      val g = fillAlias0()
      if (g.binding == this.binding) this
      else g.fillAlias()
    }

    private[this] def fillAlias0(): Graph = {
      val filler = unassignedAliases.flatMap { from => aliases(from).revealOpt(binding).map(from -> _) }
      copy(binding = binding ++ filler)
    }

    private final def infer0(): Graph = {
      // TODO: weaken with visibility
      val newBinding =
        unassignedValues
          .filterNot(hasUnassignedIncomingEdge)
          .filterNot(aliases.contains)
          .foldLeft(binding) { (b, v) =>
            val p = Pred.and(incomingEdges(v).map(_.lhs).map(_.reveal(b)).toSeq)
            b + (v -> p)
          }
      copy(binding = newBinding)
    }
  }
  object Graph {
    def build(env: Env): Graph = new Graph(Seq(), Map(), Map(), Nil, env)
  }
}
