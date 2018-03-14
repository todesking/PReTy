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
  with Solvers
  with Debugging {
  def toAST(valueRepo: ValueRepo, t: Tree): Seq[AST.CTODef]
  def reportError(pos: Pos, msg: String): Unit

  private[this] def unk(t: AST): Nothing =
    throw new RuntimeException(s"Unknown AST: $t")
  private[this] def dprint(s: String) = if (query.isDebugMode) println(s)

  def checkRefinements(root: Tree): Unit = {
    val ctos = toAST(world.values, root)
    ctos.foreach(analyzeCTO)
  }

  lazy val world = new World

  def analyzeCTO(cto: AST.CTODef): Unit = {
    dprint(s"Analyzing CTO: ${cto.pretty.toString(0)}")
    val graph = cto.impl.foldLeft(Graph.build(Env.empty)) { (g, i) => buildGraph(g, i, false) }

    def pos(v: Value) = world.values.getPos(v) match {
      case Some(p) =>
        s"${query.lineNum(p)}:${query.columnNum(p)}"
      case None =>
        s"???"
    }

    dprint(s"Initial binding:")
    graph.binding.toSeq
      .foreach {
        case (v, p) =>
          dprint(f"${pos(v)}%-7s $v = $p")
      }

    dprint("Constraints:")
    dprint(graph.constraints.mkString("\n"))

    val inferred = graph.infer()
    dprint(s"Inferred binding:")
    (inferred.binding.keySet -- graph.binding.keySet).toSeq
      .foreach {
        case v =>
          dprint(f"${pos(v)}%-7s $v = ${inferred.binding(v)}")
      }

    dprint("Unbound:")
    (inferred.allValues -- inferred.binding.keySet).toSeq
      .foreach {
        case v =>
          dprint(f"${pos(v)}%-7s $v")
      }
    dprint("Ground constraints:")
    inferred.groundConstraints.foreach { c =>
      dprint(f"${pos(c.focus)}%-7s $c")
    }

    val conflicts = new Solver(world).solve(inferred)
    conflicts.foreach { c =>
      dprint(s"CONFLICT at ${pos(c.focus)}: ${c.message}")
      val p = world.values.getPos(c.focus) getOrElse query.emptyPos
      reportError(p, c.message)
    }
  }

  def buildGraph(graph: Graph, t: AST.InImpl, inLocal: Boolean): Graph = t match {
    case AST.CTODef(impl) => unk(t)

    case AST.ValDef(sym, tpe, body) =>
      // TODO: Use default binding if public
      val template = world.templates.get(sym, graph.currentEnv)
      val g = if (inLocal) graph.let(query.name(sym), template.ret) else graph
      // TODO: distinct local val and member
      body.fold(g) { b =>
        buildGraph(g, b, true)
          .subtype(b.value, template.ret)
      }.bind(template.bindings)

    case AST.FunDef(sym, tpe, body) =>
      if (query.isAccessor(sym)) {
        graph
      } else {
        // TODO: Use default binding if public
        val template = world.templates.get(sym, graph.currentEnv)
        // TODO: Handle local def
        body.fold(graph) { b =>
          buildGraph(graph, b, true)
            .subtype(b.value, template.ret)
        }.bind(template.bindings)
      }

    case AST.Block(tpe, value, stats, expr) =>
      val intro = stats.foldLeft(graph.pushEnv()) { case (g, s) => buildGraph(g, s, inLocal) }
      buildGraph(intro, expr, inLocal)
        .subtype(expr.value, value)
        .popEnv()

    case AST.This(tpe, value) =>
      graph.bind(Map(value -> Pred.True))

    case AST.Apply(self, sym, tpe, value, argss) =>
      val g = graph.pushEnv().visible(argss.flatten.map(_.value): _*)
      // TODO: register template.binding (or make global binding repo) for foreign members
      val template = world.templates.get(sym, g.currentEnv)
      val g1 = buildGraph(g, self, inLocal)
      val g2 = argss.flatten.foldLeft(g1) { (g, a) => buildGraph(g, a, inLocal) }
      template.apply(g2, self.value, value, argss.map(_.map(_.value)))
        .popEnv()

    case AST.LocalRef(sym, tpe, value) =>
      val fv = world.values.functionValue(sym)
      graph.subtype(fv.ret, value)

    case AST.Super(tpe, value) =>
      // TODO: we can do something here
      graph

    case AST.IntLiteral(value, lit) =>
      graph.bind(Map(
        value -> Pred.exactInt(world, value, lit))).visible(value)

    case AST.UnitLiteral(value) =>
      graph.bind(Map(value -> Pred.True))

    case AST.If(value, cond, thenp, elsep) =>
      val g1 = buildGraph(graph.pushEnv, cond, true).popEnv
      val g2 = buildGraph(g1.pushEnv.cond(cond.value), thenp, true).popEnv
      val g3 = buildGraph(g2.pushEnv.condNot(cond.value), elsep, true).popEnv
      g3
  }
}

