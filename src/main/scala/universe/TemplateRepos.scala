package com.todesking.prety.universe

import com.todesking.prety.{ Template, Lang, Pred }

trait TemplateRepos { self: ForeignTypes with Queries with ValueRepos =>
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
      val asts = srcs.flatMap(Lang.parse)
      buildTemplate(f, asts)
    }

    private[this] def buildTemplate(f: DefSym, preds: Seq[(String, Lang.AST)]): Template = {
      // TODO: Check unknown pred target
      val self = valueRepo.getOrRegisterThis(f)
      val paramss = query.paramss(f)
        .map { ps => ps.map { p => valueRepo.registerParam(f, p) } }
      // When f is local val, ret is already registered as param
      val ret = valueRepo.getOrRegisterReturn(f)

      val env = query.paramss(f).zip(paramss).flatMap {
        case (ps, vs) =>
          ps.zip(vs).map { case (p, v) => query.name(p) -> v }
      }.toMap ++ Map("this" -> self, "_" -> ret)

      val bindings = preds
        .groupBy(_._1)
        .toMap
        .mapValues(_.map(_._2))
        .mapValues { asts =>
          Pred.and(asts.map(Pred.Expr(_, env)))
        }.map {
          case (k, v) =>
            env(k) -> v
        }
      Template(self, ret, paramss, bindings)
    }
  }
}
