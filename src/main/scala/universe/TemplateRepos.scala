package com.todesking.prety.universe

import com.todesking.prety.Lang

trait TemplateRepos { self: ForeignTypes with Queries with Values with ValueRepos with Templates with Preds with Envs with Debugging =>
  val templateRepo = new TemplateRepo

  class TemplateRepo {
    def get(f: DefSym): Template = {
      templates.get(f).getOrElse {
        val t = freshTemplate(f, buildEnv(Map.empty))
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
        val srcs = query.refinementSrc(f)
        val defs = Lang.parse(srcs)
        buildTemplate(f, defs, env)
      }
    }

    private[this] def buildTemplate(f: DefSym, preds: Map[String, Lang.Def], baseEnv: Env): Template = {
      // TODO: Check unknown pred target
      val self = valueRepo.getOrRegisterThis(f)
      val paramss: Seq[Seq[(String, Value)]] = query.paramss(f)
        .map { ps => ps.map { p => (query.name(p), valueRepo.registerParam(f, p)) } }
      // When f is local val, ret is already registered as param
      val ret = valueRepo.getOrRegisterReturn(f)

      val env: Env = baseEnv.bind(paramss.flatten.toMap + ("this" -> self))

      val bindings =
        if (query.isLocal(f) && preds.isEmpty) {
          Map.empty[Value, Pred]
        } else {
          paramss.flatten.map(_._2).map { v => v -> Pred.True }.toMap ++ preds
            .map {
              case (k, v) =>
                val target = if (k == "_") ret else env.findValue(k)
                val pred = Pred.compile(v.props, target.tpe, env)
                target -> pred
            }
        }
      Template(self, ret, paramss, bindings)
    }
  }
}
