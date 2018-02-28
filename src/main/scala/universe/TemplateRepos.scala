package com.todesking.prety.universe

import com.todesking.prety.Lang

trait TemplateRepos { self: ForeignTypes with Queries with Values with ValueRepos with Templates with Preds =>
  val templateRepo = new TemplateRepo

  class TemplateRepo {
    def get(f: DefSym): Template = {
      templates.get(f).getOrElse {
        val t = freshTemplate(f)
        this.templates = templates + (f -> t)
        t
      }
    }

    private[this] var templates = Map.empty[DefSym, Template]

    private[this] def freshTemplate(f: DefSym): Template = {
      val srcs = query.refinementSrc(f)
      val defs = Lang.parse(srcs)
      buildTemplate(f, defs)
    }

    private[this] def buildTemplate(f: DefSym, preds: Map[String, Lang.Def]): Template = {
      // TODO: Check unknown pred target
      val self = valueRepo.getOrRegisterThis(f)
      val paramss = query.paramss(f)
        .map { ps => ps.map { p => valueRepo.registerParam(f, p) } }
      // When f is local val, ret is already registered as param
      val ret = valueRepo.getOrRegisterReturn(f)

      val values = query.paramss(f).zip(paramss).flatMap {
        case (ps, vs) =>
          ps.zip(vs).map { case (p, v) => query.name(p) -> v }
      }.toMap ++ Map("this" -> self)

      val env: Env = new Env(
        props = Map(),
        values = values,
        theValue = ret,
        ops = Map())

      val bindings = preds
        .map {
          case (k, v) =>
            val target = env.findValue(k)
            val pred = Pred.compile(v.props, target.tpe, env)
            target -> pred
        }
      Template(self, ret, paramss, bindings)
    }
  }
}
