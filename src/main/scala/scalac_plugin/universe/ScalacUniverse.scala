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

  override def funName(f: FunSym) = f.name.toString

  // TODO: What means of "decoded"? I don't know
  override def funParamNames(f: FunSym): Seq[Seq[String]] =
    f.asMethod.paramLists.map { _.map { sym => sym.name.decoded } }

  override def refinementSrcFromFun(f: FunSym): Seq[String] =
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
            Value.fresh(s"fun:$name"),
            vparamss.map(_.map(parseValDef)),
            if (rhs.isEmpty) None else Some(parseExpr(rhs))))
      case vd @ ValDef(mods, name, tpt, rhs) =>
        Seq(parseValDef(vd))
      case other =>
        Seq(parseExpr(other))
    }

    def parseValDef(t: Tree): AST.ValDef = t match {
      case vd @ ValDef(mods, name, tpt, rhs) =>
        AST.ValDef(vd.symbol, tpt.symbol.selfType, Value.fresh(s"val:$name"), if (rhs.isEmpty) None else Some(parseExpr(rhs)))
      case unk => unknown("ValDef", unk)
    }

    def parseExpr(t: Tree): AST.Expr = t match {
      case b @ Block(stats, expr) =>
        AST.Block(b.tpe, Value.fresh("{...}"), stats.flatMap(parseImpl), parseExpr(expr))
      case Apply(fun, args) =>
        val (self, funSym, tpe) = parseFun(fun)
        AST.Apply(self, funSym, tpe, Value.fresh(t.toString), Seq(args.map(parseExpr)))
      case t @ This(qual) =>
        AST.This(t.tpe, Value.fresh())
      case Literal(Constant(v)) =>
        v match {
          case i: Int => AST.IntLiteral(Value.fresh(s"lit:$i"), i)
          case u: Unit => AST.UnitLiteral(Value.fresh(s"lit:()"))
        }
      case sel @ Select(qual, name) =>
        val target = parseExpr(qual)
        AST.Select(sel.tpe, Value.fresh(s"sel:$name"), target, sel.symbol)
      case s @ Super(qual, mix) =>
        AST.Super(s.tpe, Value.fresh(s.toString))
      case i @ Ident(name) =>
        AST.ValRef(i.symbol, i.tpe, Value.fresh(s"ref:$name"))
      case unk => unknown("Expr", unk)
    }

    def parseFun(fun: Tree): (AST.Expr, FunSym, TypeSym) = fun match {
      case sel @ Select(qual, name) =>
        val funSym = sel.symbol.asMethod
        val funType = funSym.returnType
        val self = parseExpr(qual)
        (self, funSym, funType)
    }
  }

  private[this] def unknown(kind: String, t: global.Tree) =
    throw new RuntimeException(s"Unsupported tree[$kind]: $t(${t.getClass.getName})")

}
