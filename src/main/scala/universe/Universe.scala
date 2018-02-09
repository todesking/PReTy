package com.todesking.prety.universe

import com.todesking.prety.{ Pred, Graph, Solver, Lang, Value }

trait Universe extends AnyRef
  with ForeignTypes
  with ASTs
  with Queries
  with ValueRepos
  with TemplateRepos {
  def toAST(t: Tree): Seq[AST.CTODef]
  def emptyPos: Pos
  def reportError(pos: Pos, msg: String): Unit

  def pos(v: Value): Pos =
    valueRepo.getPos(v) getOrElse emptyPos

  def templateOf(f: DefSym) =
    templateRepo.get(f)

  private[this] def unk(t: AST): Nothing =
    throw new RuntimeException(s"Unknown AST: $t")

  def checkRefinements(root: Tree): Unit = {
    val ctos = toAST(root)
    ctos.foreach(analyzeCTO)
  }

  def analyzeCTO(cto: AST.CTODef): Unit = {
    println(s"Analyzing CTO: ${cto.pretty.toString(0)}")
    val graph = Graph.merge(cto.impl.map(buildGraph))
    val inferred = graph.infer()
    println(inferred.constraints.mkString("\n"))
    println(s"Initial:")
    graph.binding.toSeq.sortBy(_._1.id)
      .foreach {
        case (v, p) =>
          println(s"  $v = $p")
      }
    println(s"Inferred:")
    (inferred.binding.keySet -- graph.binding.keySet).toSeq
      .sortBy(_.id)
      .foreach {
        case v =>
          println(s"  $v = ${inferred.binding(v)}")
      }
    println("Unbound:")
    (inferred.allValues -- inferred.binding.keySet).toSeq
      .sortBy(_.id)
      .foreach {
        case v =>
          println(s"  $v")
      }

    val conflicts = Solver.solve(inferred)
    conflicts.foreach { c =>
      println(s"CONFLICT: ${"???"}: ${c.message}")
      reportError(null, c.message)
    }
  }

  def buildGraph(t: AST.InImpl): Graph = t match {
    case AST.CTODef(impl) => unk(t)

    case AST.ValDef(sym, tpe, value, body) =>
      // TODO: Use default binding if public
      val template = templateOf(sym)
      body.fold(Graph.empty) { b =>
        Graph
          .constraint(b.value *<:= value)
          .merge(buildGraph(b))
          .constraint(b.value *<:= template.ret)
      }.bind(template.bindings)

    case AST.FunDef(sym, tpe, body) =>
      // TODO: Use default binding if public
      val template = templateOf(sym)
      body.fold(Graph.empty) { b =>
        Graph.constraint(b.value *<:= template.ret)
          .merge(buildGraph(b))
      }.bind(template.bindings)

    case AST.Block(tpe, value, stats, expr) =>
      Graph.merge(stats.map(buildGraph))
        .merge(buildGraph(expr))
        .constraint(expr.value *<:= value)

    case AST.This(tpe, value) =>
      Graph.bind(value, Pred.True)

    case AST.Apply(self, sym, tpe, value, argss) =>
      // TODO: register template.binding (or make global binding repo) for foreign members
      val template = templateOf(sym)
      buildGraph(self)
        .merge(argss.flatten.map(buildGraph))
        .merge(template.apply(self.value, value, argss.map(_.map(_.value))))

    case AST.ValRef(sym, tpe, value) =>
      // TODO: add equality
      Graph.empty

    case AST.Super(tpe, value) =>
      // TODO: we can do something here
      Graph.empty

    case AST.IntLiteral(value, lit) =>
      Graph.bind(
        value,
        Pred.Expr(
          Lang.AST.Op(Lang.AST.TheValue, "==", Lang.AST.LitInt(lit)),
          Map()))

    case AST.UnitLiteral(value) =>
      Graph.bind(value, Pred.True)
  }

}

