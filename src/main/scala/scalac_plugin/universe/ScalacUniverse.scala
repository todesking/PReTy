package com.todesking.prety.scalac_plugin.universe

import scala.tools.nsc.{ Global }

class ScalacUniverse[G <: Global](val global: G) extends Universe {
  override type Tree = global.Tree
  override type Pos = global.Position
  override type FunSym = global.Symbol
  override type ValSym = global.Symbol
  override type TypeSym = global.Type

  override def reportError(pos: Pos, msg: String): Unit = {
    global.reporter.error(pos, msg)
  }

  private[this] var templates = Map.empty[FunSym, Template]
  override def templateOf(f: FunSym): Template = {
    templates.get(f).fold {
      val t = freshTemplate(f)
      this.templates = templates + (f -> t)
      t
    }(identity)
  }

  private[this] def freshTemplate(f: FunSym): Template = {
    val srcs = refinementSrcFromFun(f)
    val asts = srcs.flatMap(Lang.parse)
    buildTemplate(f, asts)
  }

  private[this] def buildTemplate(f: FunSym, asts: Seq[Lang.AST.Pred]): Template = {
    val self = Value.fresh()
    val paramss = funParamNames(f).map { ns => ns.map { _ => Value.fresh() } }
    val nameToValue = funParamNames(f).zip(paramss).flatMap {
      case (ns, vs) =>
        ns.zip(vs).map { case (n, v) => (n -> v) }
    }.toMap
    val ret = Value.fresh()
    val (sp, pp, rp) = buildPreds(self, nameToValue, ret, asts)

    Template(self, ret, paramss)
  }

  private[this] def buildPreds(
    self: Value, nameToValue: Map[String, Value], ret: Value, asts: Seq[Lang.AST.Pred]): (Pred, Map[String, Pred], Pred) = {
    // TODO: check acyclicity
    val env = nameToValue ++ Map("this" -> self)
    val compiled: Map[String, Pred] = asts
      .map { a =>
        a.id -> compilePred(env, a.expr)
      }
      .groupBy(_._1)
      .toMap
      .mapValues(_.map(_._2))
      .mapValues(Pred.and)
    return (
      compiled.getOrElse("this", Pred.True),
      compiled.filter { case (k, _) => k != "_" && k != "this" },
      compiled.getOrElse("_", Pred.True))
  }

  private[this] def compilePred(env: Map[String, Value], expr: Lang.AST.Expr): Pred = ???

  // TODO: What means of "decoded"? I don't know
  private[this] def funParamNames(f: FunSym): Seq[Seq[String]] =
    f.asMethod.paramLists.map { _.map { sym => sym.name.decoded } }

  private[this] def refinementSrcFromFun(f: FunSym): Seq[String] =
    refinementSrc(f)

  private[this] val annotationTpe = global.rootMirror.getRequiredClass("com.todesking.prety.refine").tpe
  private[this] def refinementSrc(sym: global.Symbol): Seq[String] = sym match {
    case s: global.TermSymbol =>
      import global._
      s.annotations.collect {
        case global.Annotation(tpe, sargs, jargs) if tpe <:< annotationTpe =>
          if (sargs.size != 1) throw new AssertionError(s"Expected exact 1 arguments: $sargs")
          sargs(0) match {
            case Literal(Constant(src: String)) => src
          }
      }
    case unk =>
      throw new RuntimeException(s"Unsupported symbol: $sym(${sym.getClass.getName})")
  }

  override def toAST(t: Tree): Seq[AST.CTODef] =
    TreeParser.parseTop(t)

  object TreeParser {
    import global.{ Tree => _, _ }
    def parseTop(t: Tree): Seq[AST.CTODef] = t match {
      case PackageDef(pid, stats) =>
        stats.flatMap(parseTop)
      case ModuleDef(mods, name, impl) =>
        Seq(AST.CTODef(impl.body.flatMap(parseImpl)))
      case Import(expr, selectors) =>
        Seq()
      case unk => unknown("Top", unk)
    }

    def parseImpl(t: Tree): Seq[AST.InImpl] = t match {
      case Import(expr, selectors) =>
        Seq()
      case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val sym = dd.symbol.asMethod
        Seq(
          AST.FunDef(
            sym,
            sym.returnType,
            Value.fresh(),
            vparamss.map(_.map(parseValDef)),
            if (rhs.isEmpty) None else Some(parseExpr(rhs))))
      case other =>
        Seq(parseExpr(other))
    }

    def parseValDef(t: Tree): AST.ValDef = t match {
      case vd @ ValDef(mods, name, tpt, rhs) =>
        AST.ValDef(vd.symbol, tpt.symbol.selfType, Value.fresh(), if (rhs.isEmpty) None else Some(parseExpr(rhs)))
      case unk => unknown("ValDef", unk)
    }

    def parseExpr(t: Tree): AST.Expr = t match {
      case b @ Block(stats, expr) =>
        AST.Block(b.tpe, Value.fresh(), stats.flatMap(parseImpl), parseExpr(expr))
      case Apply(fun, args) =>
        val (self, funSym, tpe) = parseFun(fun)
        AST.Apply(self, funSym, tpe, Value.fresh(), Seq(args.map(parseExpr)))
      case t @ This(qual) =>
        AST.This(t.tpe, Value.fresh())
      case Literal(Constant(v)) =>
        v match {
          case i: Int => AST.IntLiteral(Value.fresh(), i)
          case u: Unit => AST.UnitLiteral(Value.fresh())
        }
      case unk => unknown("Expr", unk)
    }

    def parseFun(fun: Tree): (AST.Expr, FunSym, TypeSym) = fun match {
      case sel @ Select(qual, name) =>
        val funSym = sel.symbol.asMethod
        val funType = funSym.returnType
        val self =
          qual match {
            case Super(q, mix) =>
              parseExpr(q)
          }
        (self, funSym, funType)
    }
  }

  private[this] def unknown(kind: String, t: global.Tree) =
    throw new RuntimeException(s"Unsupported tree[$kind]: $t(${t.getClass.getName})")

}
