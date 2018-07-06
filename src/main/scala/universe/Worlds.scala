package com.todesking.prety.universe

import com.todesking.prety.Lang
import scala.reflect.runtime.{universe => ru}

trait Worlds { self: ForeignTypes with Values with Templates with Props with Exprs with Envs with Preds with Macros with Debugging=>
  class Cache[K, V](calc: K => V) {
    private[this] var cache = Map.empty[K, V]
    def apply(k: K): V = cache.get(k) getOrElse {
      val v = calc(k)
      cache = cache + (k -> v)
      v
    }
  }
  object Cache {
    def apply[K, V](f: K => V): Cache[K, V] = new Cache(f)
  }
  class Cache1[K, A1, V](calc: (K, A1) => V) {
    private[this] var cache = Map.empty[K, V]
    def apply(k: K, a1: A1): V = cache.get(k) getOrElse {
      val v = calc(k, a1)
      cache = cache + (k -> v)
      v
    }
  }
  object Cache1 {
    def apply[K, A1, V](f: (K, A1) => V): Cache1[K, A1, V] = new Cache1(f)
  }

  class PredQuery(world: World) {
    def compile(tpe: TypeSym, ast: Lang.Pred, env: Map[String, Expr]): Pred = {
      world.defaultPred(tpe).custom(
        ast.self.map(Expr.compile(world.macroEnv, env, _, tpe)),
        ast.props.map {
          case (name, p) =>
            val key = world.propKey(tpe, name) getOrElse {
              throw new RuntimeException(s"Property $name not found in $tpe")
            }
            key -> compile(key.tpe, p, env)
        })
    }

    def exactInt(v: Int): Pred =
      world.defaultPred(query.types.int).custom(
        CoreExpr.INT_EQ(CoreExpr.TheValue(query.types.int), CoreExpr.INT_Lit(v))
      )

    def exactBoolean(v: Boolean): Pred =
      world.defaultPred(query.types.boolean).custom(
        CoreExpr.BOOL_EQ(CoreExpr.TheValue(query.types.boolean), CoreExpr.BOOL_Lit(v))
      )
  }

  object Facade {
  }
  class World(
    customProps: Map[TypeSym, Prop],
    customMembers: Map[DefSym, MemberDef],
    val macroEnv: MacroEnv
  ){
    private[this] val templateRepo = new TemplateRepo(this)

    private[this] val valueRepo = new ValueRepo
    private[this] val defaultPreds = Cache[TypeSym, Pred] { t =>
      val keys = query.stableValueMembers(t).map { f =>
        propKey(t, query.name(f)) getOrElse {
          throw new AssertionError(s"Can't find pred key for stable member $f")
        }
      }.toSet
      Pred.Default(t, CoreExpr.True, keys, k => defaultPropPred(t,k))
    }
    private[this] val propKeyss = Cache[TypeSym, Map[String, PropKey]] { tpe =>
      query.stableValueMembers(tpe).map { f =>
        query.name(f) -> PropKey(query.name(f), query.thisType(f), query.returnType(f))
      }.toMap
    }
    private[this] val templates = Cache[DefSym, Template] { f =>
      templateRepo.get(f)
    }
    private[this] val localTemplates = Cache1[DefSym, Env, Template] { (f, env) =>
      templateRepo.registerLocal(f, env)
    }
    private[this] val functionValuess = Cache[DefSym, FunctionValue] { f =>
      valueRepo.functionValue(f)
    }
    private[this] val props = Cache[TypeSym, Prop] { tpe =>
      // TODO: [BUG] types are partial order. need tsort
      customProps.values.filter(tpe <:< _.tpe).toSeq.sortWith { (a, b) => a.tpe <:< b.tpe }.headOption getOrElse {
        // TODO: return default prop
        throw new RuntimeException(s"Property for $tpe not found: env=${customProps.values.mkString(", ")}")
      }
    }

    val values = new ValueRepo

    val preds = new PredQuery(this)

    // def propKeys(t: TypeSym): Set[PropKey] = propKeyss(t)
    def propKey(t: TypeSym, name: String): Option[PropKey] = propKeyss(t).get(name)
    def template(f: DefSym): Template = templates(f)
    def localTemplate(f: DefSym, env: Env): Template = localTemplates(f, env)
    def functionValue(f: DefSym): FunctionValue = functionValuess(f)
    def prop(tpe: TypeSym): Prop = props(tpe)
    def defaultPred(t: TypeSym): Pred = defaultPreds(t)
    def memberMakro(t: TypeSym, name: String, argTypes: Seq[Seq[TypeSym]]): Option[Macro] =
      Some(query.lookupMember(t, name, query.types.any, argTypes)).flatMap { f =>
        template(f).makro
      }

    private[this] def defaultPropPred(tpe: TypeSym, key: PropKey): Pred = {
      query.stableValueMembers(tpe).find { f => query.name(f) == key.name }.map { f =>
        val t = template(f)
        t.bindings(t.ret)
      } getOrElse {
        throw new AssertionError(s"Can't find prop $key in $tpe")
      }
    }
  }

  case class MemberDef(
    paramNames: Seq[Seq[String]],
    predAST: Map[String, Lang.Pred],
    makroAST: Option[Lang.Expr]
  )

  class Configuration {
    import scala.collection.{mutable => mu}
    private[this] val props = mu.Map.empty[TypeSym, Prop]
    private[this] val makros = mu.Map.empty[String, Macro]
    private[this] val memberDefs = mu.Map.empty[DefSym, MemberDef]

    def newWorld(): World = {
      val macroMembers = memberDefs.flatMap { case (f, d) =>
        d.makroAST.map { ast =>
          f -> Macro.method(
            query.name(f),
            ast,
            query.returnType(f),
            d.paramNames.zip(query.paramss(f).map(_.map(query.returnType(_)))).map{ case (n, t) => n.zip(t) }
          )
        }
      }.toMap
      new World(
        props.toMap,
        memberDefs.toMap,
        new MacroEnv(makros.toMap, macroMembers)
      )
    }

    def registerProp(p: Prop): Unit = {
      if (props.contains(p.tpe))
        throw new RuntimeException(s"Prop conflict: $p")
      props +=  (p.tpe -> p)
    }

    def registerMacro(m: Macro): Unit = {
      if(makros.contains(m.name))
        throw new RuntimeException(s"Name conflict: ${m.name}")
      makros += (m.name -> m)
    }

    def registerMember(selfName: String, methodName: String, retName: String, paramNames: Seq[Seq[(String, String)]], src: String): Unit = {
      val selfT = query.types.fromName(selfName)
      val retT = query.types.fromName(retName)
      val paramss = paramNames.map(_.map { case (n, t) => (n, query.types.fromName(t))})
      registerMember(selfT, methodName, retT, paramss, Seq(src), Seq())
    }

    def registerMember(selfT: TypeSym, methodName: String, retT: TypeSym, paramss: Seq[Seq[(String, TypeSym)]], srcs: Seq[String], simples: Seq[String]): Unit = {
      dprint(s"registering ${selfT}.$methodName ${paramss.map(_.map {case (k,v) => s"$k: $v" }).mkString("(", ", ", ")")} ${srcs.mkString(", ")}|${simples.mkString(", ")}")
      if (simples.nonEmpty && srcs.nonEmpty) throw new RuntimeException("@refine and @refine.simple is exclusive")
      if (simples.size > 1) throw new RuntimeException("Multiple @refine.simple")

      // TODO: Integrate with template.freshTemplate()

      val f = query.lookupMember(selfT, methodName, retT, paramss.map(_.map(_._2)))
      val predAST =
        if(simples.isEmpty) Lang.parse(srcs)
          else Lang.parseSingle(s"_: @core.eq(_, ${simples.head})")
      val makroAST = simples.headOption.map(Lang.parseExpr)
      memberDefs += (f -> MemberDef(paramss.map(_.map(_._1)), predAST, makroAST))
      // val env = Env(name2value)
      // val value2pred =
      //   name2value.map { case (k, v) => name2value(k) -> preds.default(v.tpe) }.toMap ++ defs.map {
      //         case (name, d) =>
      //           name2value(name) -> preds.compile(name2type(name), d, env)
      //       }
      // val makro = simples.headOption.map { s =>
      //   Macro.method(this, methodName, s, retT, paramss)
      // }

      // // TODO: dup to Templates/buildTemplate
      // val propKey =
      //   if(query.isStable(f)) Some(PropKey(query.name(f), query.thisType(f), query.returnType(f)))
      //   else None
      // templates.register(f, value2pred, makro, propKey)
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

  class MemberEnv {
    private[this] var entities = Map.empty[TypeSym, Map[String, PropKey]]

    def propKey(tpe: TypeSym, name: String): PropKey =
      propKeys(tpe)(name)

    def propKeys(tpe: TypeSym): Map[String, PropKey] =
      entities.get(tpe) getOrElse {
        entities = entities + (tpe -> findPropKeys(tpe))
        entities(tpe)
      }

    private[this] def findPropKeys(tpe: TypeSym): Map[String, PropKey] = {
      query.stableValueMembers(tpe).map { mem =>
        val name = query.name(mem)
        name -> PropKey(name, tpe, query.returnType(mem))
      }.toMap
    }

    def add(tpe: TypeSym, name: String, key: PropKey): Unit = {
      entities = entities + (tpe -> (propKeys(tpe) + (name -> key)))
    }
  }

  object World {
    def buildDefault(): World = {
      val conf = new Configuration
      conf.registerProp(new BooleanProp)
      conf.registerProp(new IntProp)
      conf.registerProp(new AnyRefProp)

      conf.registerMacro(CoreLib)

      conf.registerMembers(ru.typeTag[BooleanProxy])
      conf.registerMembers(ru.typeTag[IntProxy])

      conf.newWorld()
    }

    import scala.language.reflectiveCalls
    private[this] val T = query.types
    private[this] def opI(name: String, f: {def apply(l: CoreExpr, r:CoreExpr): CoreExpr}): (String, Macro) =
      name -> Macro.fun(s"core.int.$name", T.int, T.int) { case (_, Seq(l: CoreExpr, r: CoreExpr)) =>
        f.apply(l, r)
      }
    private[this] def opB(name: String, f: {def apply(l: CoreExpr, r:CoreExpr): CoreExpr}): (String, Macro) =
      name -> Macro.fun(s"core.bool.$name", T.boolean, T.boolean) { case (_, Seq(l: CoreExpr, r: CoreExpr)) =>
        f.apply(l, r)
      }

    val genericEq = Macro.fun("core.eq", T.any, T.any) {
      case (_, Seq(l: CoreExpr, r: CoreExpr)) =>
        if(l.tpe <:< T.int && r.tpe <:< T.int)
          CoreExpr.INT_EQ(l, r)
        else if(l.tpe <:< T.boolean && r.tpe <:< T.boolean)
          CoreExpr.BOOL_EQ(l, r)
        else throw new RuntimeException(s"core.eq: Illegal argument: $l, $r")
    }
    val CoreLib = Macro.dict("core")(
      "eq" -> genericEq,
      "int" -> Macro.dict("core.int")(
          opI("eq", CoreExpr.INT_EQ),
          opI("lt", CoreExpr.INT_LT),
          opI("gt", CoreExpr.INT_GT),
          opI("ge", CoreExpr.INT_GE),
          opI("div", CoreExpr.INT_DIV),
          opI("mul", CoreExpr.INT_MUL),
          opI("plus", CoreExpr.INT_PLUS),
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

      @refine.simple("@core.int.plus(this, x)")
      def +(x: Int): Int
      // @refine("_: _ == @core.int.minus(this, x)")
      def -(x: Int): Int
      @refine("_: _ == @core.int.mul(this, x)")
      def *(x: Int): Int
      @refine.simple("@core.int.div(this, x)")
      def /(x: Int): Int
      // @refine("_: _ == @core.int.mod(this, x)")
      def %(x: Int): Int
    }
  }
}
