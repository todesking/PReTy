package com.todesking.prety.universe

import com.todesking.prety.Lang
import scala.reflect.runtime.{universe => ru}

trait Worlds { self: ForeignTypes with Values with Templates with Props with Exprs with Envs with Preds with Macros with Debugging=>
  class World() {
    private[this] def nf(kind: String, key: String) =
      throw new RuntimeException(s"$kind $key not found")

    private[this] var propKeys = Map.empty[String, PropKey]
    private[this] var props = Map.empty[TypeSym, Prop]
    private[this] var macros = Map.empty[String, Macro]

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

    def registerMacro(m: Macro): Unit = {
      if(macros.contains(m.name))
        throw new RuntimeException(s"Name conflict: ${m.name}")
      macros = macros + (m.name -> m)
    }

    def findMethodMacro(selfT: TypeSym, name: String, argss: Seq[Seq[TypeSym]]): Macro = {
      val f = query.lookupMember(selfT, name, query.types.any, argss)
      val t = templates.get(f)
      ???
    }

    def findMacro(name: String): Macro = macros.get(name) getOrElse nf("Macro", name)

    def findPropKey(name: String, targetType: TypeSym): PropKey = name match {
      case "_" =>
        PropKey.Self
      case name =>
        propKeys.get(name) getOrElse nf("Property", name)
    }

    def findProp(tpe: TypeSym): Prop =
      props.get(tpe) getOrElse nf("Prop", tpe.toString)

    val templates = new TemplateRepo(this)
    val values = new ValueRepo

    def registerMember(selfName: String, methodName: String, retName: String, paramNames: Seq[Seq[(String, String)]], src: String): Unit = {
      val selfT = query.types.fromName(selfName)
      val retT = query.types.fromName(retName)
      val paramss = paramNames.map(_.map { case (n, t) => (n, query.types.fromName(t))})
      registerMember(selfT, methodName, retT, paramss, Seq(src))
    }

    def registerMember(selfT: TypeSym, methodName: String, retT: TypeSym, paramss: Seq[Seq[(String, TypeSym)]], src: Seq[String]): Unit = {
      dprint(s"registering ${selfT}.$methodName ${paramss.map(_.map {case (k,v) => s"$k: $v" }).mkString("(", ", ", ")")} ${src.mkString(", ")}")
      val f = query.lookupMember(selfT, methodName, retT, paramss.map(_.map(_._2)))
      val fv = values.functionValue(f)
      val name2value = paramss.flatten.map(_._1).zip(fv.paramss.flatten.map(_._2)).toMap + ("this" -> fv.self) + ("_" -> fv.ret)
      val name2type = paramss.flatten.toMap + ("this" -> selfT) + ("_" -> fv.ret.tpe)
      val env = Env(name2value)
      val value2pred =
        (Map(fv.self -> Pred.True, fv.ret -> Pred.True) ++ fv.paramss.flatten.map(_._2 -> Pred.True)) ++ Lang.parse(src).map {
          case (name, d) =>
            val v = name2value(name)
            v -> Pred.compile(this, d.props, name2type(name), env)
        }
      templates.register(f, value2pred)
    }

    def registerMembers(proxy: ru.TypeTag[_]): Unit = {
      val proxyAnnotationType = ru.typeOf[com.todesking.prety.refine.proxy]
      val refineAnnotationType = ru.typeOf[com.todesking.prety.refine]
      def proxyName(t: ru.Type): Option[String] = {
        val names =
          t.typeSymbol.annotations.collect {
            case an if an.tree.tpe <:< proxyAnnotationType =>
              an.tree.children.tail(0).asInstanceOf[ru.Literal].value.asInstanceOf[String]
          }
        if(names.size > 1) throw new RuntimeException(s"too many proxy names: $names")
        names.headOption
      }
      val targetName =proxyName(proxy.tpe) getOrElse { throw new RuntimeException(s"proxy annotation not found: $proxy") }
      val selfType = query.types.fromName(targetName)
      def unproxy(t: ru.Type): TypeSym =
        proxyName(t).fold{ query.types.fromName(t.typeSymbol.fullName) } { n => query.types.fromName(n) }
      def refinementSrc(m: ru.MethodSymbol): Seq[String] = {
        val names =
          m.annotations.collect {
            case an if an.tree.tpe <:< refineAnnotationType =>
              an.tree.children.tail(0).asInstanceOf[ru.Literal].value.asInstanceOf[String]
          }
        names
      }
      proxy.tpe.decls.foreach { sym =>
        if(sym.isMethod) {
          val m = sym.asMethod
          registerMember(
            selfType,
            m.name.decodedName.toString,
            unproxy(m.returnType),
            m.paramLists.map(_.map { p => p.name.decodedName.toString -> unproxy(p.asTerm.info)}),
            refinementSrc(m)
          )
        } else {
          throw new RuntimeException(s"Unsupported member in proxy: $sym")
        }
      }
    }
  }

  object World {
    def buildDefault(): World = {
      val w = new World
      w.registerProp(new BooleanProp)
      w.registerProp(new IntProp)

      w.registerMacro(CoreLib)

      w.registerMembers(ru.typeTag[IntProxy])

      w
    }

    import scala.language.reflectiveCalls
    private[this] val T = query.types
    private[this] def op(name: String, f: {def apply(l: CoreExpr, r:CoreExpr): CoreExpr}): (String, Macro) =
      name -> Macro.fun(s"core.int.$name", T.int, T.int) { case Seq(l: CoreExpr, r: CoreExpr) =>
        f.apply(l, r)
      }

    val CoreLib = Macro.dict("core")(
      "int" -> Macro.dict("core.int")(
          op("eq", CoreExpr.INT_EQ),
          op("lt", CoreExpr.INT_LT),
          op("gt", CoreExpr.INT_GT),
      )
    )

    import com.todesking.prety.refine
    @refine.proxy("scala.Int")
    trait IntProxy {
      def unary_~ : Int
      def unary_+ : Int
      def unary_- : Int

      @refine("_: @core.int.eq(this, x)")
      def ==(x: Int): Boolean
      // @refine("_: !@core.int.eq(this, x)")
      def !=(x: Int): Boolean

      @refine.simple("@core.int.lt(this, x)")
      def <(x: Int): Boolean
      // @refine("_: @core.int.le(this, x)")
      def <=(x: Int): Boolean
      @refine.simple("@core.int.gt(this, x)")
      def >(x: Int): Boolean
      // @refine("_: @core.int.ge(this, x)")
      def >=(x: Int): Boolean

      def |(x: Int): Int
      def &(x: Int): Int
      def ^(x: Int): Int

      // @refine("_: _ == @core.int.plus(this, x)")
      // @refine.expr("@core.int.plus(this, x)")
      def +(x: Int): Int
      // @refine("_: _ == @core.int.minus(this, x)")
      def -(x: Int): Int
      // @refine("_: _ == @core.int.mult(this, x)")
      def *(x: Int): Int
      // @refine("_: _ == @core.int.div(this, x)")
      def /(x: Int): Int
      // @refine("_: _ == @core.int.mod(this, x)")
      def %(x: Int): Int
    }
  }
}
