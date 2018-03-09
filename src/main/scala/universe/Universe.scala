package com.todesking.prety.universe

trait Universe extends AnyRef
  with ForeignTypes
  with Queries
  with ForeignTypeOps
  with Constraints
  with Conflicts
  with Values
  with ValueRepos
  with ASTs
  with Preds
  with Worlds
  with UnknownPreds
  with Envs
  with Exprs
  with Props
  with Graphs
  with Templates
  with TemplateRepos
  with Solvers {
  def toAST(t: Tree): Seq[AST.CTODef]
  def reportError(pos: Pos, msg: String): Unit

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
    val graph = cto.impl.foldLeft(Graph.empty) { (g, i) => buildGraph(g, i) }
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
      val p = valueRepo.getPos(c.focus) getOrElse query.emptyPos
      println(s"CONFLICT at ${c.focus}: ${c.message}")
      reportError(p, c.message)
    }
  }

  def buildGraph(graph: Graph, t: AST.InImpl): Graph = t match {
    case AST.CTODef(impl) => unk(t)

    case AST.ValDef(sym, tpe, body) =>
      // TODO: Use default binding if public
      val template = templateOf(sym)
      // TODO: distinct local val and member
      body.fold(graph) { b =>
        buildGraph(graph, b)
          .subtype(b.value, template.ret)
      }.bind(template.bindings)

    case AST.FunDef(sym, tpe, body) =>
      // TODO: Use default binding if public
      val template = templateOf(sym)
      // TODO: Handle local def
      body.fold(graph) { b =>
        val g = graph.subtype(b.value, template.ret)
        buildGraph(g, b)
      }.bind(template.bindings)

    case AST.Block(tpe, value, stats, expr) =>
      val intro = stats.foldLeft(graph) { (g, s) => buildGraph(g, s) }
      buildGraph(intro, expr)
        .subtype(expr.value, value)

    case AST.This(tpe, value) =>
      graph.bind(Map(value -> Pred.True))

    case AST.Apply(self, sym, tpe, value, argss) =>
      // TODO: register template.binding (or make global binding repo) for foreign members
      val template = templateOf(sym)
      val g1 = buildGraph(graph, self)
      val g2 = argss.flatten.foldLeft(g1) { (g, a) => buildGraph(g, a) }
      template.apply(g2, self.value, value, argss.map(_.map(_.value)))

    case AST.LocalRef(sym, tpe, value) =>
      // TODO: add equality
      graph

    case AST.Super(tpe, value) =>
      // TODO: we can do something here
      graph

    case AST.IntLiteral(value, lit) =>
      graph.bind(Map(
        value -> Pred.exactInt(value, lit)))

    case AST.UnitLiteral(value) =>
      graph.bind(Map(value -> Pred.True))
  }

}

