package com.todesking.prety.universe

trait TypeCheckers { self: ForeignTypes with Values with Templates with Worlds with Envs with ASTs with Preds with Values with Debugging with Graphs with Constraints with Conflicts with Solvers =>
  class TypeChecker(world: World) {
    def check(cto: AST.CTODef): Seq[Conflict] = {
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

      dprint("Value dependencies")
      graph.allValues.map(_.naked).toSeq.foreach { v =>
        dprint(" ", v)
        graph.incomingEdges(v).map(_.lhs.dependency).toSeq.foreach { d =>
          dprint("    >", d)
        }
      }

      val inferred = graph.infer(world)
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
      conflicts
    }

    private[this] def unk(t: AST): Nothing =
      throw new RuntimeException(s"Unknown AST: $t")

    def buildGraph(graph: Graph, t: AST.InImpl, inLocal: Boolean): Graph = t match {
      case AST.CTODef(impl) => unk(t)

      case AST.ValDef(sym, body) =>
        // TODO: Use default binding if public
        val template = if (inLocal) world.templates.registerLocal(sym, graph.currentEnv) else world.templates.get(sym)
        val g = if (inLocal) graph.let(query.name(sym), template.ret) else graph
        // TODO: distinct local val and member
        body.fold(g) { b =>
          buildGraph(g, b, true)
            .subtype(b.value, template.ret)
        }.bind(template.bindings)

      case AST.FunDef(sym, body) =>
        // TODO: Local def is forward-referenciable: Need two-phase traverse?
        if (query.isAccessor(sym)) {
          graph
        } else {
          // TODO: Use default binding if public
          val template = if (inLocal) world.templates.registerLocal(sym, graph.currentEnv) else world.templates.get(sym)
          // TODO: Handle local def
          body.fold(graph) { b =>
            buildGraph(graph, b, true)
              .subtype(b.value, template.ret)
          }.bind(template.bindings)
        }

      case AST.Block(value, stats, expr) =>
        val intro = stats.foldLeft(graph.pushEnv()) { case (g, s) => buildGraph(g, s, inLocal) }
        buildGraph(intro, expr, inLocal)
          .subtype(expr.value, value)
          .popEnv()

      case AST.This(value) =>
        graph.bind(value -> world.preds.default(value.tpe))

      case AST.Apply(self, sym, value, argss) =>
        val g = graph.pushEnv()
        // TODO: register template.binding (or make global binding repo) for foreign members
        val template = world.templates.get(sym)
        val g1 = buildGraph(g, self, inLocal)
        val g2 = argss.flatten.foldLeft(g1) { (g, a) => buildGraph(g, a, inLocal) }
        template.apply(g2, self.value, value, argss.map(_.map(_.value)))
          .popEnv()

      case AST.LocalRef(sym, value) =>
        graph

      case AST.PackageRef(sym, value) =>
        graph.bind(value -> world.preds.default(value.tpe))

      case AST.Super(value) =>
        // TODO: we can do something here
        graph

      case AST.IntLiteral(value, lit) =>
        graph

      case AST.BooleanLiteral(value, lit) =>
        graph

      case AST.UnitLiteral(value) =>
        graph.bind(value -> world.preds.default(value.tpe))

      case AST.If(value, cond, thenp, elsep) =>
        val g1 = buildGraph(graph.pushEnv, cond, true).popEnv
        val g2 = buildGraph(g1.pushEnv.cond(cond.value), thenp, true).popEnv
        val g3 = buildGraph(g2.pushEnv.condNot(cond.value), elsep, true).popEnv
        g3

      case AST.New(v) =>
        graph.bind(v -> world.preds.default(v.tpe))
    }
  }
}
