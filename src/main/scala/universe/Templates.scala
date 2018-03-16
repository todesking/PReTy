package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Templates { self: ForeignTypes with Preds with Graphs with Values with Worlds with Envs with Macros =>
  case class Template(
    self: Value,
    ret: Value,
    argss: Seq[Seq[(String, Value)]],
    bindings: Map[Value, Pred],
    makro: Option[Macro]) {
    override def toString =
      s"$self.(${argss.map(_.map { case (_, x) => s"${x}: ${bindings.get(x) getOrElse Pred.True}" }.mkString("(", ", ", ")")).mkString("")}) = $ret: ${bindings.get(ret) getOrElse Pred.True}; $bindings"
    // TODO: check acyclic
    def apply(
      graph: Graph,
      aSelf: Value,
      aRet: Value,
      aArgss: Seq[Seq[Value]]): Graph = {
      require(argss.map(_.size) == aArgss.map(_.size))

      val argSub = Map(self -> aSelf) ++
        argss.flatten.zip(aArgss.flatten).map { case ((n, p), a) => p -> a }

      // TODO: tsort args
      argss.flatten.zip(aArgss.flatten).foldLeft {
        graph
          .bind(bindings)
          .pushEnv()
          .subtype(aSelf, self)
          .let("this", aSelf)
      } {
        case (g, ((name, p), a)) =>
          g.subtype(a, p.substitute(argSub)).let(name, a)
      }.subtypeR(ret.substitute(argSub), aRet)
        .popEnv()
    }
  }

  class TemplateRepo(world: World) {
    def register(f: DefSym, binding: Map[Value, Pred], makro: Option[Macro]): Unit = {
      if (templates.contains(f))
        throw new RuntimeException(s"register: Conflict: $f")
      // TODO: check preds type
      val fv = world.values.functionValue(f)
      templates = templates + (f -> Template(fv.self, fv.ret, fv.paramss, binding, makro))
    }

    def get(f: DefSym): Template = {
      templates.get(f).getOrElse {
        val t = freshTemplate(f, Env.empty)
        this.templates = templates + (f -> t)
        t
      }
    }

    def registerLocal(f: DefSym, env: Env): Template = {
      if (templates.contains(f))
        throw new RuntimeException(s"registerLocal: Conflict: $f")
      freshTemplate(f, env)
    }

    private[this] var templates = Map.empty[DefSym, Template]

    private[this] def freshTemplate(f: DefSym, env: Env): Template = {
      if (query.isAccessor(f)) {
        get(query.unwrapAccessor(f))
      } else {
        val srcs = query.refineAnnotations(f)
        val simples = query.refineSimpleAnnotations(f)
        if (simples.nonEmpty && srcs.nonEmpty) throw new RuntimeException("@refine and @refine.simple is exclusive")
        if (simples.size > 1) throw new RuntimeException("Multiple @refine.simple")
        val defs =
          if (simples.nonEmpty) Lang.parseSingle(s"_: _ == ${simples.head}")
          else Lang.parse(srcs)
        val makro = simples
          .headOption
          .map { src =>
            val paramss = query.paramss(f).map(_.map { p => query.name(p) -> query.returnType(p) })
            Macro.method(world, query.name(f), src, query.returnType(f), paramss)
          }
        buildTemplate(f, defs, env, makro)
      }
    }

    private[this] def buildTemplate(f: DefSym, preds: Map[String, Lang.Def], baseEnv: Env, makro: Option[Macro]): Template = {
      // TODO: Check unknown pred target
      val fv = world.values.functionValue(f)

      val env: Env = baseEnv.bind(fv.paramss.flatten.toMap + ("this" -> fv.self))

      val bindings =
        if (query.isLocal(f) && preds.isEmpty) {
          // If local member has no refinement spec, let infer it later.
          Map.empty[Value, Pred]
        } else {
          Map(fv.ret -> Pred.True, fv.self -> Pred.True) ++ fv.paramss.flatten.map(_._2).map { v => v -> Pred.True }.toMap ++ preds
            .map {
              case (k, v) =>
                val target = if (k == "_") fv.ret else env.findValue(k)
                val pred = Pred.compile(world, v.props, target.tpe, env)
                target -> pred
            }
        }
      Template(fv.self, fv.ret, fv.paramss, bindings, makro)
    }
  }
}
