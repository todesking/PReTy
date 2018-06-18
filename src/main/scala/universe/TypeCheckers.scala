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
        dprint(f"${pos(c.focus)}%-7s ${c.focus.shortString}%-3s: $c")
      }

      dprint("Constraints with AST")
      locally {
        val cs = inferred.groundConstraints
          .groupBy(_.focus)
          .toMap
        def str(v: Value): String = {
          s"${v.shortString} ${cs.get(v) map (_.mkString(", ")) orElse inferred.binding.get(v.naked) getOrElse "???"}"
        }
        def show(ast: AST, level: Int): Unit = {
          def p(args: Any*) = dprint(Seq(" " * (level * 2)) ++ args: _*)
          import AST._
          ast match {
            case CTODef(impl) =>
              p("CTO")
              impl.foreach(show(_, level + 1))
            case Block(value, stmts, expr) =>
              p("Block", str(value))
              stmts.foreach(show(_, level + 1))
              show(expr, level + 1)
            case This(v) =>
              p("This", str(v))
            case Apply(self, sym, value, argss) =>
              p("App", str(value))
              argss.foreach(_.foreach(show(_, level + 1)))
            case LocalRef(sym, value) =>
              p("Ref", str(value))
            case PackageRef(sym, value) =>
              p("Package", str(value))
            case Super(v) =>
              p("Super", str(v))
            case lit: Literal =>
              p("Lit", str(lit.value))
            case New(v) =>
              p("New", str(v))
            case If(v, c, th, el) =>
              p("If", str(v))
              show(c, level + 1)
              p("Then")
              show(th, level + 1)
              p("Else")
              show(el, level + 1)
            case FunDef(sym, body) =>
              val t = world.templates.get(sym)
              p("FunDef", sym)
              t.argss.foreach { args =>
                args.foreach {
                  case (name, value) =>
                    p("-", str(value))
                }
              }
              body.foreach(show(_, level + 1))
            case ValDef(sym, body) =>
              val t = world.templates.get(sym)
              p("ValDef", sym, str(t.ret))
              body.foreach(show(_, level + 1))
          }
        }
        show(cto, 0)
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
            val base = buildGraph(graph, b, true)
            if (query.isConstructor(sym)) base
            else base.subtype(b.value, template.ret)
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
        val g1 = buildGraph(g.template(template), self, inLocal)
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
