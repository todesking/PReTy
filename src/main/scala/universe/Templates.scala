package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Templates { self: ForeignTypes with Preds with Graphs with Values with Worlds with Envs with Macros with Props with Debugging with Exprs =>
  case class Template(
    self: Value,
    ret: Value,
    argss: Seq[Seq[(String, Value)]],
    bindings: Map[Value, Pred],
    propKey: Option[PropKey]) {
    override def toString =
      s"$self.(${argss.map(_.map { case (_, x) => s"${x}: ${bindings.get(x).map(_.toString) getOrElse "(none)"}" }.mkString("(", ", ", ")")).mkString("")}) = $ret: ${bindings.get(ret).map(_.toString) getOrElse "(none)"}; $bindings"
    // TODO: check acyclic
    def apply(
      graph: Graph,
      aSelf: Value,
      aRet: Value,
      aArgss: Seq[Seq[Value]]): Graph = {
      require(argss.map(_.size) == aArgss.map(_.size))

      val argSub = Map(self -> aSelf) ++
        argss.flatten.zip(aArgss.flatten).map { case ((n, p), a) => p -> a }

      val g =
        propKey.fold {
          graph.subtypeR(ret.substitute(argSub), aRet)
        } { k =>
          // TODO: if prop(aSelf) <:< (self), p will just aSelf.prop(k)
          val p = aSelf.prop(k) & self.substitute(argSub).prop(k)
          graph
            .subtypeR(p, aRet)
            .subtypeR(p, ret)
        }

      // TODO: tsort args
      argss.flatten.zip(aArgss.flatten).foldLeft {
        g
          .bind(bindings)
          .pushEnv()
          .subtype(aSelf, self)
          .let("this", aSelf)
      } {
        case (g, ((name, p), a)) =>
          g.subtype(a, p.substitute(argSub)).let(name, a)
      }.popEnv()
      // Technically, exact constraint is ret =:= aRet.
      // Due to aRet is unbound, use ret <:< aRet for refinement inference.
    }
  }

  class TemplateRepo(world: World) {
    def register(f: DefSym, binding: Map[Value, Pred], propKey: Option[PropKey]): Unit = {
      if (templates.contains(f))
        throw new RuntimeException(s"register: Conflict: $f")
      // TODO: check preds type
      val fv = world.values.functionValue(f)
      templates = templates + (f -> Template(fv.self, fv.ret, fv.paramss, binding, propKey))
    }

    def get(f: DefSym): Template = {
      val naked = unwrap(f)
      templates.get(naked).getOrElse {
        val t = freshTemplate(naked, Env.empty)
        this.templates = templates + (naked -> t)
        t
      }
    }

    def registerLocal(f: DefSym, env: Env): Template = {
      val naked = unwrap(f)
      if (templates.contains(naked))
        throw new RuntimeException(s"registerLocal: Conflict: $naked")
      val t = freshTemplate(naked, env)
      templates = templates + (naked -> t)
      t
    }

    private[this] def unwrap(f: DefSym) =
      if (query.isAccessor(f)) query.unwrapAccessor(f) else f

    private[this] var templates = Map.empty[DefSym, Template]

    private[this] def freshTemplate(f: DefSym, env: Env): Template = {
      val srcs = query.refineAnnotations(f)
      val simples = query.refineSimpleAnnotations(f)
      if (simples.nonEmpty && srcs.nonEmpty) throw new RuntimeException("@refine and @refine.simple is exclusive")
      if (simples.size > 1) throw new RuntimeException("Multiple @refine.simple")
      val defs =
        if (simples.nonEmpty) Lang.parseSingle(s"_: _ == ${simples.head}")
        else Lang.parse(srcs)
      buildTemplate(f, defs, env)
    }

    private[this] def buildTemplate(f: DefSym, preds: Map[String, Lang.Pred], baseEnv: Env): Template = {
      // TODO: Check unknown pred target
      val fv = world.values.functionValue(f)

      val env: Env = baseEnv.bind(fv.paramss.flatten.toMap + ("this" -> fv.self))

      val bindings =
        if (query.isLocal(f) && preds.isEmpty) {
          // If local member has no refinement spec, let infer it later.
          Map.empty[Value, Pred]
        } else {
          (Seq(fv.ret, fv.self) ++ fv.paramss.flatten.map(_._2)).map { v => v -> world.defaultPred(v.tpe) }.toMap ++ preds
            .map {
              case (k, v) =>
                val target = if (k == "_") fv.ret else env.findValue(k)
                val pred = world.preds.compile(target.tpe, v, env.binding.mapValues(CoreExpr.ValueRef.apply))
                target -> pred
            }
        }
      val propKey =
        if (query.paramss(f).isEmpty) world.propKey(query.thisType(f), query.name(f))
        else None
      Template(fv.self, fv.ret, fv.paramss, bindings, propKey)
    }
  }
}
