package com.todesking.prety.scalac_plugin.universe

trait Universe extends AnyRef
  with ForeignTypes
  with ASTs
  with Preds
  with Constraints
  with Inferences {
  private[this] def unk(t: AST): Nothing =
    throw new RuntimeException(s"Unknown AST: $t")

  def checkRefinements(root: Tree): Unit = {
    val ctos = toAST(root)
    ctos.foreach(analyzeCTO)
  }

  def analyzeCTO(cto: AST.CTODef): Unit = {
    val constraints = cto.impl.flatMap(gatherConstraints)
    val binding = infer(constraints)
    val conflicts = solve(constraints, binding)
    conflicts.foreach { c =>
      reportError(c.pos, c.message)
    }
  }

  def gatherConstraints(t: AST.InImpl): Seq[Constraint] = t match {
    case AST.CTODef(impl) => unk(t)
    case AST.ValDef(sym, tpe, value, body) =>
      body.fold(Seq.empty[Constraint]) { b =>
        Seq(b.value *<:= value) ++ exprConstraints(b)
      }
    case AST.FunDef(sym, tpe, value, paramss, body) =>
      body.fold(Seq.empty[Constraint]) { b =>
        Seq(b.value *<:= value) ++ exprConstraints(b)
      }
    case e: AST.Expr =>
      exprConstraints(e)
  }

  def exprConstraints(e: AST.Expr): Seq[Constraint] = e match {
    case AST.Block(tpe, value, stats, expr) =>
      stats.flatMap(gatherConstraints) ++ exprConstraints(expr)
    case AST.IntLiteral(value, lit) =>
      Seq()
    case AST.UnitLiteral(value) =>
      Seq()
    case AST.This(tpe, value) =>
      Seq()
    case AST.ValRef(sym, tye, value) =>
      Seq()
    case AST.Apply(self, sym, tpe, value, argss) =>
      val selfCs = exprConstraints(self)
      val argCs = argss.flatMap(_.flatMap(exprConstraints))
      val applyCs = templateOf(sym)
        .apply(self.value, value, argss.map(_.map(_.value)))
      selfCs ++ argCs ++ applyCs
  }

  def templateOf(sym: FunSym): Template

  case class Template(
    self: Value,
    ret: Value,
    argss: Seq[Seq[Value]]) {
    // TODO: check acyclic
    def apply(
      aSelf: Value,
      aRet: Value,
      aArgss: Seq[Seq[Value]]): Seq[Constraint] = {
      require(argss.map(_.size) == aArgss.map(_.size))

      val argSub = Map(self -> aSelf) ++
        argss.flatten.zip(aArgss.flatten).map { case (p, a) => p -> a }

      (aSelf *<:= self) +: argss.flatten.zip(aArgss.flatten).map {
        case (p, a) =>
          a *<:= p.substitute(argSub)
      } :+ (aRet *<:= ret.substitute(argSub))
    }
  }

  case class BoundValue(value: Value, pred: Pred)

  def solve(cs: Seq[Constraint], binding: Map[Value, Pred]): Seq[Conflict] = {
    ???
  }
  def infer(constraints: Seq[Constraint]): Map[Value, Pred] = {
    val g = new Graph(constraints, Map()).infer()
    if (g.unassignedValues.nonEmpty) {
      throw new RuntimeException(s"Infer failed")
    }
    g.binding
  }

  class Graph(cs: Seq[Constraint], val binding: Map[Value, Pred]) {
    lazy val allValues = cs.flatMap(_.values).toSet
    lazy val unassignedValues = allValues -- binding.keySet

    def hasUnassignedIncomingEdge(v: Value): Boolean = ???

    def incomingEdges(v: Value): Set[Constraint] = ???

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
      new Graph(cs, newBinding)
    }
  }
  class Env {
    def isVisibleFrom(v: Value, from: Value): Boolean = ???
  }

  def reportError(pos: Pos, msg: String): Unit

  case class Conflict(pos: Pos, message: String)
}

