package com.todesking.prety.universe

import com.todesking.prety.Lang

trait TemplateRepos { self: ForeignTypes with Queries with Values with ValueRepos with Templates with Preds with Envs with Debugging =>
  class TemplateRepo(world: World) {
    def register(f: DefSym, binding: Map[Value, Pred]): Unit = {
      if (templates.contains(f))
        throw new RuntimeException(s"register: Conflict: $f")
      // TODO: check preds type
      val fv = world.values.functionValue(f)
      templates = templates + (f -> Template(fv.self, fv.ret, fv.paramss, binding))
    }

    def get(f: DefSym, env: Env): Template = {
      templates.get(f).getOrElse {
        val t = freshTemplate(f, env)
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
        get(query.unwrapAccessor(f), env)
      } else {
        val srcs = query.refinementSrc(f)
        val defs = Lang.parse(srcs)
        buildTemplate(f, defs, env)
      }
    }

    private[this] def buildTemplate(f: DefSym, preds: Map[String, Lang.Def], baseEnv: Env): Template = {
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
      Template(fv.self, fv.ret, fv.paramss, bindings)
    }
  }
}
