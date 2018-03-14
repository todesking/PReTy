package com.todesking.prety.universe

import com.todesking.prety.Lang

trait TemplateRepos { self: ForeignTypes with Queries with Values with ValueRepos with Templates with Preds with Envs with Debugging =>
  lazy val templateRepo = new TemplateRepo

  class TemplateRepo {
    def register(f: DefSym, binding: Map[Value, Pred]): Unit = {
      if (templates.contains(f))
        throw new RuntimeException(s"register: Conflict: $f")
      // TODO: check preds type
      val fv = valueRepo.functionValue(f)
      templates = templates + (f -> Template(fv.self, fv.ret, fv.paramss, binding))
    }

    def get(f: DefSym, env: Env): Template = {
      templates.get(f).getOrElse {
        val t = freshTemplate(f, env)
        this.templates = templates + (f -> t)
        t
      }
    }

    private[this] def registerDefaults(): Unit = {
      reg("scala.Int", "<", "scala.Boolean", Seq(Seq("rhs" -> "scala.Int")), "_: this < rhs")
      reg("scala.Int", ">", "scala.Boolean", Seq(Seq("rhs" -> "scala.Int")), "_: this > rhs")

      def reg(selfName: String, methodName: String, retName: String, paramNames: Seq[Seq[(String, String)]], src: String) = {
        val selfT = query.types.fromName(selfName)
        val retT = query.types.fromName(retName)
        val paramssT = paramNames.map(_.map(_._2).map(query.types.fromName))
        val f = query.lookupMember(selfT, methodName, retT, paramssT)
        val fv = valueRepo.functionValue(f)
        val name2value = paramNames.flatten.map(_._1).zip(fv.paramss.flatten.map(_._2)).toMap + ("this" -> fv.self) + ("_" -> fv.ret)
        val name2type = paramNames.flatten.map { case (name, t) => name -> query.types.fromName(t) }.toMap + ("this" -> selfT) + ("_" -> fv.ret.tpe)
        val env = buildEnv(name2value)
        val value2pred =
          (Map(fv.self -> Pred.True, fv.ret -> Pred.True) ++ fv.paramss.flatten.map(_._2 -> Pred.True)) ++ Lang.parseSingle(src).map {
            case (name, d) =>
              val v = name2value(name)
              v -> Pred.compile(d.props, name2type(name), env)
          }
        register(f, value2pred)
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
      val fv = valueRepo.functionValue(f)

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
                val pred = Pred.compile(v.props, target.tpe, env)
                target -> pred
            }
        }
      Template(fv.self, fv.ret, fv.paramss, bindings)
    }

    registerDefaults()
  }
}
