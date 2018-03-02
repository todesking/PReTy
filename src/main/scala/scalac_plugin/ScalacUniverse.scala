package com.todesking.prety.scalac_plugin

import scala.tools.nsc.Global

import com.todesking.prety.universe.Universe

class ScalacUniverse[G <: Global](val global: G) extends Universe {
  override type Tree = global.Tree
  override type Pos = global.Position

  override type DefSym = global.TermSymbol

  override type TypeSym = global.Type

  override def reportError(pos: Pos, msg: String): Unit = {
    global.reporter.error(pos, msg)
  }

  override val query = new QueryAPI {
    override def name(f: DefSym) = f.name.toString
    override def paramss(f: DefSym): Seq[Seq[DefSym]] = f.paramLists.map(_.map(_.asTerm))
    override def returnType(f: DefSym): TypeSym =
      if (f.isMethod) f.asMethod.returnType
      else f.selfType
    override def thisType(f: DefSym): TypeSym =
      f.thisType
    override def refinementSrc(f: DefSym) = {
      import global._
      f.annotations.collect {
        case global.Annotation(tpe, sargs, jargs) if tpe <:< annotationTpe =>
          if (sargs.size != 1) throw new AssertionError(s"Expected exact 1 arguments: $sargs")
          sargs(0) match {
            case Literal(Constant(src: String)) => src
          }
      }
    }

    override val emptyPos = scala.reflect.internal.util.NoPosition

    override def <:<(lhs: TypeSym, rhs: TypeSym) = lhs <:< rhs

    override val types = new TypesAPI {
      private[this] def get(name: String) = global.rootMirror.getRequiredClass(name).tpe
      override val nothing = get("scala.Nothing")
      override val int = get("scala.Int")
      override val boolean = get("scala.Boolean")
    }

  }

  private[this] val annotationTpe = global.rootMirror.getRequiredClass("com.todesking.prety.refine").tpe

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
        val template = templateOf(sym)
        valueRepo.setPos(template.self, t.pos)
        valueRepo.setPos(template.ret, t.pos)
        for {
          (vs, ts) <- template.argss.zip(vparamss)
          (v, t) <- vs.zip(ts)
        } {
          valueRepo.setPos(v, t.pos)
        }
        // TODO: remove values from def/ref AST: Symbols is enough
        // TODO: Handle default args
        Seq(
          AST.FunDef(
            sym,
            sym.returnType,
            if (rhs.isEmpty) None else Some(parseExpr(rhs))))
      case vd @ ValDef(mods, name, tpt, rhs) =>
        val sym = vd.symbol.asTerm
        Seq(AST.ValDef(sym, sym.selfType, if (rhs.isEmpty) None else Some(parseExpr(rhs))))
      case other =>
        Seq(parseExpr(other))
    }

    def parseExpr(t: Tree): AST.Expr = t match {
      case b @ Block(stats, expr) =>
        AST.Block(
          b.tpe,
          valueRepo.newExpr("{...}", t.pos, b.tpe),
          stats.flatMap(parseImpl),
          parseExpr(expr))
      case Apply(fun, args) =>
        val (self, funSym, tpe) = parseFun(fun)
        AST.Apply(self, funSym, tpe, valueRepo.newExpr(t.toString, t.pos, t.tpe), Seq(args.map(parseExpr)))
      case t @ This(qual) =>
        AST.This(t.tpe, valueRepo.newExpr(s"this", t.pos, t.tpe))
      case Literal(Constant(v)) =>
        v match {
          case i: Int => AST.IntLiteral(valueRepo.newExpr(s"lit:$i", t.pos, t.tpe), i)
          case u: Unit => AST.UnitLiteral(valueRepo.newExpr(s"lit:()", t.pos, t.tpe))
        }
      case sel @ Select(qual, name) =>
        val target = parseExpr(qual)
        val sym = sel.symbol.asTerm
        AST.Apply(target, sym, sel.tpe, valueRepo.newExpr(sel.toString, t.pos, t.tpe), Seq())
      case s @ Super(qual, mix) =>
        AST.Super(s.tpe, valueRepo.newExpr(s.toString, t.pos, t.tpe))
      case i @ Ident(name) =>
        AST.LocalRef(i.symbol.asTerm, i.tpe, valueRepo.newExpr(s"ref:${i.symbol}", t.pos, t.tpe))
      case unk => unknown("Expr", unk)
    }

    private[this] def parseFun(fun: Tree): (AST.Expr, DefSym, TypeSym) = fun match {
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
