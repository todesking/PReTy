package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Envs { self: ForeignTypes with ForeignTypeOps with Queries with Values with TemplateRepos with ValueRepos with Props with Preds with Exprs with Props =>

  case class Env(
    binding: Map[String, Value],
    unnamed: Set[Value],
    conds: Set[Value],
    unconds: Set[Value]) {

    def values: Set[Value] = binding.values.toSet ++ unnamed

    private[this] def vnf(key: String) =
      throw new RuntimeException(s"Value $key not found(env: ${binding.keys.mkString(", ")})")

    def findValue(name: String): Value =
      binding.get(name) getOrElse vnf(name)

    def bind(mapping: Map[String, Value]): Env =
      bind(mapping.toSeq: _*)
    def bind(mapping: (String, Value)*): Env =
      copy(binding = binding ++ mapping)

    def bindUnnamed(v: Value*): Env =
      copy(unnamed = unnamed ++ v)

    def cond(v: Value): Env =
      copy(conds = conds + v)
    def uncond(v: Value): Env =
      copy(unconds = unconds + v)
  }
  object Env {
    val empty: Env =
      Env(Map(), Set(), Set(), Set())
    def apply(binding: Map[String, Value]): Env =
      empty.copy(binding = binding)
  }

  class World() {
    private[this] def nf(kind: String, key: String) =
      throw new RuntimeException(s"$kind $key not found")

    private[this] var propKeys = Map.empty[String, PropKey]
    private[this] var props = Map.empty[TypeSym, Prop]

    def registerProp(p: Prop): Unit = {
      if (props.contains(p.tpe))
        throw new RuntimeException(s"Prop conflict: $p")
      props = props + (p.tpe -> p)
    }

    def registerPropKey(k: PropKey.Named): Unit = {
      if (propKeys.contains(k.name))
        throw new RuntimeException(s"Name conflict: ${k.name}")
      propKeys = propKeys + (k.name -> k)
    }

    def findPropKey(name: String, targetType: TypeSym): PropKey = name match {
      case "_" =>
        PropKey.Self
      case name =>
        propKeys.get(name) getOrElse nf("Property", name)
    }

    def findProp(tpe: TypeSym): Prop =
      props.get(tpe) getOrElse nf("Prop", tpe.toString)

    def findOp(tpe: TypeSym, name: String): (Expr, Expr) => Expr = {
      import query.{ types => T }
      if (tpe <:< query.types.int)
        name match {
          case ">" => {
            case (l: CoreExpr, r: CoreExpr) if l.tpe <:< T.int && r.tpe <:< T.int =>
              CoreExpr.INT_GT(l, r)
          }
          case "<" => {
            case (l: CoreExpr, r: CoreExpr) if l.tpe <:< T.int && r.tpe <:< T.int =>
              CoreExpr.INT_LT(l, r)
          }
          case "==" => {
            case (l: CoreExpr, r: CoreExpr) if l.tpe <:< T.int && r.tpe <:< T.int =>
              CoreExpr.INT_EQ(l, r)
          }
          case unk =>
            nf("Operator", s"$tpe.$name")
        }
      else nf("Operator", s"$tpe.$name")
    }

    val templates = new TemplateRepo(this)
    val values = new ValueRepo

    private[this] def registerDefaults(): Unit = {
      registerProp(new BooleanProp)
      registerProp(new IntProp)

      reg("scala.Int", "<", "scala.Boolean", Seq(Seq("rhs" -> "scala.Int")), "_: this < rhs")
      reg("scala.Int", ">", "scala.Boolean", Seq(Seq("rhs" -> "scala.Int")), "_: this > rhs")

      def reg(selfName: String, methodName: String, retName: String, paramNames: Seq[Seq[(String, String)]], src: String) = {
        val selfT = query.types.fromName(selfName)
        val retT = query.types.fromName(retName)
        val paramssT = paramNames.map(_.map(_._2).map(query.types.fromName))
        val f = query.lookupMember(selfT, methodName, retT, paramssT)
        val fv = values.functionValue(f)
        val name2value = paramNames.flatten.map(_._1).zip(fv.paramss.flatten.map(_._2)).toMap + ("this" -> fv.self) + ("_" -> fv.ret)
        val name2type = paramNames.flatten.map { case (name, t) => name -> query.types.fromName(t) }.toMap + ("this" -> selfT) + ("_" -> fv.ret.tpe)
        val env = Env(name2value)
        val value2pred =
          (Map(fv.self -> Pred.True, fv.ret -> Pred.True) ++ fv.paramss.flatten.map(_._2 -> Pred.True)) ++ Lang.parseSingle(src).map {
            case (name, d) =>
              val v = name2value(name)
              v -> Pred.compile(this, d.props, name2type(name), env)
          }
        templates.register(f, value2pred)
      }
    }

    registerDefaults()

  }

  object World {
  }
}
