package com.todesking.prety

class Graph(
  val constraints: Seq[Constraint],
  val binding: Map[Value, Pred]) {
  def +(rhs: Graph) = {
    val conflicts = this.binding.keySet intersect rhs.binding.keySet
    if (conflicts.nonEmpty) {
      throw new RuntimeException(s"Binding conflict: ${conflicts.mkString(", ")}")
    }
    new Graph(
      constraints ++ rhs.constraints,
      binding ++ rhs.binding)
  }

  def merge(rhs: Graph) = this + rhs
  def merge(rhs: Seq[Graph]) = this + Graph.merge(rhs)

  def constraint(c: Constraint) =
    this + Graph.constraint(c)
  def constraint(cs: Seq[Constraint]) =
    this + Graph.constraint(cs)
  def bind(vps: Map[Value, Pred]) =
    this + Graph.bind(vps)

  lazy val allValues = constraints.flatMap(_.values).toSet
  lazy val assignedValues = binding.keySet
  lazy val unassignedValues = allValues -- assignedValues

  // TODO: check unbound values
  def groundConstraints: Seq[GroundConstraint] =
    constraints.map { c =>
      GroundConstraint(c.lhs.pred(binding), c.rhs.pred(binding))
    }

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
          val p = Pred.and(incomingEdges(v).map(_.lhs).map(_.pred(b)).toSeq)
          b + (v -> p)
        }
    new Graph(constraints, newBinding)
  }
}
object Graph {
  val empty: Graph = new Graph(Seq(), Map())

  def merge(gs: Seq[Graph]): Graph =
    gs.foldLeft(empty) { (a, x) => a + x }
  def constraint(c: Constraint): Graph =
    new Graph(Seq(c), Map())
  def constraint(cs: Seq[Constraint]): Graph =
    new Graph(cs, Map())
  def bind(v: Value, p: Pred): Graph =
    new Graph(Seq(), Map(v -> p))
  def bind(vps: Map[Value, Pred]): Graph =
    new Graph(Seq(), vps)
}

