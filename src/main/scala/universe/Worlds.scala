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
      t.makro getOrElse { throw new RuntimeException(s"$selfT.${query.name(f)}${argss.map(_.mkString("(", ", ", ")"))} can't use in expr") }
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
      registerMember(selfT, methodName, retT, paramss, Seq(src), Seq())
    }

    def registerMember(selfT: TypeSym, methodName: String, retT: TypeSym, paramss: Seq[Seq[(String, TypeSym)]], srcs: Seq[String], simples: Seq[String]): Unit = {
      // TODO: Integrate with template.freshTemplate()
      dprint(s"registering ${selfT}.$methodName ${paramss.map(_.map {case (k,v) => s"$k: $v" }).mkString("(", ", ", ")")} ${srcs.mkString(", ")}|${simples.mkString(", ")}")
      val f = query.lookupMember(selfT, methodName, retT, paramss.map(_.map(_._2)))
      val fv = values.functionValue(f)
      val name2value = paramss.flatten.map(_._1).zip(fv.paramss.flatten.map(_._2)).toMap + ("this" -> fv.self) + ("_" -> fv.ret)
      val name2type = paramss.flatten.toMap + ("this" -> selfT) + ("_" -> fv.ret.tpe)
      val env = Env(name2value)
      val base = Map(fv.self -> Pred.True, fv.ret -> Pred.True) ++ fv.paramss.flatten.map(_._2 -> Pred.True)
      if (simples.nonEmpty && srcs.nonEmpty) throw new RuntimeException("@refine and @refine.simple is exclusive")
      if (simples.size > 1) throw new RuntimeException("Multiple @refine.simple")
      val defs =
        if(simples.isEmpty) Lang.parse(srcs)
          else Lang.parseSingle(s"_: @core.eq(_, ${simples.head})")
      val value2pred = 
        base ++ defs.map {
              case (name, d) =>
                val v = name2value(name)
                v -> Pred.compile(this, d.props, name2type(name), env)
            }
      val makro = simples.headOption.map { s =>
        Macro.method(this, methodName, s, retT, paramss)
      }
      templates.register(f, value2pred, makro)
    }

    def registerMembers(proxy: ru.TypeTag[_]): Unit = {
      val proxyAnnotationType = ru.typeOf[com.todesking.prety.refine.proxy]
      val refineAnnotationType = ru.typeOf[com.todesking.prety.refine]
      val refineSimpleAnnotationType = ru.typeOf[com.todesking.prety.refine.simple]
      def proxyName(t: ru.Type): Option[String] = {
        val names =
          t.typeSymbol.annotations.collect {
            case an if an.tree.tpe <:< proxyAnnotationType =>
              an.tree.children.tail(0).asInstanceOf[ru.Literal].value.asInstanceOf[ru.Constant].value.asInstanceOf[String]
          }
        if(names.size > 1) throw new RuntimeException(s"too many proxy names: $names")
        names.headOption
      }
      val targetName =proxyName(proxy.tpe) getOrElse { throw new RuntimeException(s"proxy annotation not found: $proxy") }
      val selfType = query.types.fromName(targetName)
      def unproxy(t: ru.Type): TypeSym =
        proxyName(t).fold{ query.types.fromName(t.typeSymbol.fullName) } { n => query.types.fromName(n) }
      def refineSrc(m: ru.MethodSymbol): Seq[String] = {
        val names =
          m.annotations.collect {
            case an if an.tree.tpe <:< refineAnnotationType =>
              an.tree.children.tail(0).asInstanceOf[ru.Literal].value.asInstanceOf[ru.Constant].value.asInstanceOf[String]
          }
        names
      }
      def refineSimpleSrc(m: ru.MethodSymbol): Seq[String] = {
        val names =
          m.annotations.collect {
            case an if an.tree.tpe <:< refineSimpleAnnotationType =>
              an.tree.children.tail(0).asInstanceOf[ru.Literal].value.asInstanceOf[ru.Constant].value.asInstanceOf[String]
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
            refineSrc(m),
            refineSimpleSrc(m)
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

      w.registerMembers(ru.typeTag[BooleanProxy])
      w.registerMembers(ru.typeTag[IntProxy])

      w
    }

    import scala.language.reflectiveCalls
    private[this] val T = query.types
    private[this] def opI(name: String, f: {def apply(l: CoreExpr, r:CoreExpr): CoreExpr}): (String, Macro) =
      name -> Macro.fun(s"core.int.$name", T.int, T.int) { case (Seq(l: CoreExpr, r: CoreExpr), _) =>
        f.apply(l, r)
      }
    private[this] def opB(name: String, f: {def apply(l: CoreExpr, r:CoreExpr): CoreExpr}): (String, Macro) =
      name -> Macro.fun(s"core.bool.$name", T.boolean, T.boolean) { case (Seq(l: CoreExpr, r: CoreExpr), _) =>
        f.apply(l, r)
      }

    val genericEq = Macro.fun("core.eq", T.any, T.any) {
      case (Seq(l: CoreExpr, r: CoreExpr), _) =>
        if(l.tpe <:< T.int && r.tpe <:< T.int)
          CoreExpr.INT_EQ(l, r)
        else if(l.tpe <:< T.boolean && r.tpe <:< T.boolean)
          CoreExpr.BOOL_EQ(l, r)
        else throw new RuntimeException(s"core.eq: Illegal argument: $l, $r")
    }
    val intEq = opI("eq", CoreExpr.INT_EQ)._2
    val CoreLib = Macro.dict("core")(
      "eq" -> genericEq,
      "int" -> Macro.dict("core.int")(
          "eq" -> intEq,
          opI("lt", CoreExpr.INT_LT),
          opI("gt", CoreExpr.INT_GT),
          opI("ge", CoreExpr.INT_GE),
      ),
      "bool" -> Macro.dict("core.bool")(
        opB("eq", CoreExpr.BOOL_EQ),
      ),
    )

    import com.todesking.prety.refine

    @refine.proxy("scala.Boolean")
    trait BooleanProxy {
      @refine.simple("@core.bool.eq(this, x)")
      def ==(x: Boolean): Boolean
    }

    @refine.proxy("scala.Int")
    trait IntProxy {
      def unary_~ : Int
      def unary_+ : Int
      def unary_- : Int

      @refine.simple("@core.int.eq(this, x)")
      def ==(x: Int): Boolean
      // @refine("_: !@core.int.eq(this, x)")
      def !=(x: Int): Boolean

      @refine.simple("@core.int.lt(this, x)")
      def <(x: Int): Boolean
      // @refine("_: @core.int.le(this, x)")
      def <=(x: Int): Boolean
      @refine.simple("@core.int.gt(this, x)")
      def >(x: Int): Boolean
      @refine.simple("@core.int.ge(this, x)")
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
