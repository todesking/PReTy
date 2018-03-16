package com.todesking.prety.scalac_plugin

import scala.tools.nsc.Global

import com.todesking.prety.universe.Universe

class ScalacUniverse[G <: Global](val global: G, debug: Boolean) extends Universe {
  override type Tree = global.Tree
  override type Pos = global.Position

  override type DefSym = global.TermSymbol

  override type TypeSym = global.Type

  override def reportError(pos: Pos, msg: String): Unit = {
    global.reporter.error(pos, msg)
  }

  object Annotations {
    private[this] def load(name: String) = global.rootMirror.getRequiredClass(name).tpe
    val refine = load("com.todesking.prety.refine")
    val refineSimple = load("com.todesking.prety.refine.simple")
  }

  override lazy val query = new QueryAPI {
    override def name(f: DefSym) = f.name.decodedName.toString
    override def paramss(f: DefSym): Seq[Seq[DefSym]] = f.paramLists.map(_.map(_.asTerm))
    override def returnType(f: DefSym): TypeSym =
      if (f.isMethod) f.asMethod.returnType
      else f.selfType
    // TODO: Rename it: In scalac, "thisType" means "this.type".
    override def thisType(f: DefSym): TypeSym =
      if (f.owner.isType) f.owner.asType.tpe
      else if (f.owner.isModule) f.owner.asModule.moduleClass.asType.tpe
      else thisType(f.owner.asTerm)
    override def refineAnnotations(f: DefSym) = {
      f.annotations.collect {
        case an if an.tpe <:< Annotations.refine =>
          an.tree.children.tail(0).asInstanceOf[global.Literal].value.asInstanceOf[global.Constant].value.asInstanceOf[String]
      }
    }
    override def refineSimpleAnnotations(f: DefSym) = {
      f.annotations.collect {
        case an if an.tpe <:< Annotations.refineSimple =>
          an.tree.children.tail(0).asInstanceOf[global.Literal].value.asInstanceOf[global.Constant].value.asInstanceOf[String]
      }
    }
    override def pos(f: DefSym) = f.pos

    override def isAccessor(f: DefSym) = f.isAccessor
    override def unwrapAccessor(f: DefSym) = f.accessed.asTerm

    override def isLocal(f: DefSym) = f.isLocalToBlock

    override val isDebugMode = debug

    override val emptyPos = scala.reflect.internal.util.NoPosition
    override def lineNum(p: Pos) = p.focus.line
    override def columnNum(p: Pos) = p.focus.column
    override def samePos(l: Pos, r: Pos) = l == r

    override def <:<(lhs: TypeSym, rhs: TypeSym) = lhs <:< rhs

    override def lookupMembers(self: TypeSym, name: String, ret: TypeSym, paramss: Seq[Seq[TypeSym]]): Seq[DefSym] = {
      def matcher(x: global.Symbol): Boolean = {
        if (x.isMethod) {
          val m = x.asMethod
          m.name.decodedName.toString == name &&
            m.returnType <:< ret &&
            paramss.zip(m.paramLists).forall { case (ls, rs) => ls.zip(rs.map(_.info)).forall { case (l, r) => l <:< r } }
        } else {
          false
        }
      }
      self.members.sorted.filter(matcher).map(_.asTerm)
    }

    override val types = new TypesAPI {
      private[this] def get(name: String) = global.rootMirror.getRequiredClass(name).tpe
      override val nothing = get("scala.Nothing")
      override val any = get("scala.Any")
      override val int = get("scala.Int")
      override val boolean = get("scala.Boolean")
      override def fromName(fqn: String) =
        get(fqn)
    }

  }

  override def toAST(valueRepo: ValueRepo, t: Tree): Seq[AST.CTODef] =
    new TreeParser(valueRepo).parseTop(t)

  class TreeParser(valueRepo: ValueRepo) {
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
        AST.Apply(self, funSym, tpe, valueRepo.newExpr("app:" + t.toString, t.pos, t.tpe), Seq(args.map(parseExpr)))
      case t @ This(qual) =>
        AST.This(t.tpe, valueRepo.newExpr(s"this", t.pos, t.tpe))
      case Literal(Constant(v)) =>
        v match {
          case i: Int => AST.IntLiteral(valueRepo.newRef(Value.IntLiteral(i), t.pos), i)
          case u: Unit => AST.UnitLiteral(valueRepo.newExpr(s"lit:()", t.pos, t.tpe))
        }
      case sel @ Select(qual, name) =>
        val target = parseExpr(qual)
        val sym = sel.symbol.asTerm
        AST.Apply(target, sym, sel.tpe, valueRepo.newExpr("sel:" + sel.toString, sel.pos, sel.tpe), Seq())
      case s @ Super(qual, mix) =>
        AST.Super(s.tpe, valueRepo.newExpr(s.toString, t.pos, t.tpe))
      case i @ Ident(name) =>
        val fv = valueRepo.functionValue(i.symbol.asTerm)
        AST.LocalRef(i.symbol.asTerm, i.tpe, valueRepo.newRef(fv.ret, i.pos))
      case If(cond, thenp, elsep) =>
        AST.If(valueRepo.newExpr("if", t.pos, t.tpe), parseExpr(cond), parseExpr(thenp), parseExpr(elsep))
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
