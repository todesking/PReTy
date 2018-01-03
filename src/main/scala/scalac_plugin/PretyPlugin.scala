package com.todesking.prety.scalac_plugin

import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }

class PretyPlugin(override val global: Global) extends Plugin {
  override val name = "prety-scalac-plugin"
  override val description = "PReTy Scalac Plugin"
  override val components = List(new Component(global))
}

class Component(override val global: Global) extends PluginComponent {
  override val phaseName = "prety-check-phase"
  override val runsAfter = List("typer")
  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
    override def apply(unit: global.CompilationUnit): Unit = {
      val u = new ScalacUniverse[global.type](global)
      val ctos = u.parseToplevel(unit.body)

      println(u.parseToplevel(unit.body))
    }
    private[this] def pp(t: global.Tree): Unit = {
      println(global.showRaw(t))
    }
  }
}

class GenSym[A](make: Int => A) {
  private[this] var _next = 0
  private[this] def genNextId(): Int = {
    _next += 1
    _next - 1
  }
  def apply(): A = make(genNextId())
}

class ScalacUniverse[G <: Global](val global: G) {
  private[this] def unknown(kind: String, t: global.Tree) =
    throw new RuntimeException(s"Unsupported tree[$kind]: $t(${t.getClass.getName})")

  def parseToplevel(tree: global.Tree): Seq[AST.CTO] = tree match {
    case global.PackageDef(id, stats) =>
      stats.flatMap(parseToplevel)
    case i: global.Import =>
      Seq()
    case global.ModuleDef(mods, global.TermName(name), impl) =>
      impl match {
        case global.Template(parents, self, body) =>
          Seq(AST.CTO(name, body.map(parseTerm)))
        case unk =>
          unknown("Module body", unk)
      }
    case unk =>
      unknown("Tree", unk)
  }

  def parseValDef(tree: global.ValDef): AST.ValDef = tree match {
    case vd @ global.ValDef(mods, global.TermName(name), tpe, rhs) =>
      AST.ValDef(name, rtOf(vd.symbol))
    case unk =>
      unknown("ValDef", unk)
  }

  def parseTerm(tree: global.Tree): AST.Term = tree match {
    case global.DefDef(mods, global.TermName(name), tparams, vparamss, tpt, rhs) =>
      AST.DefDef(name, vparamss.map(_.map(parseValDef)), parseTerm(rhs))
    case vd: global.ValDef =>
      parseValDef(vd)
    case x =>
      parseExpr(x)
  }

  def parseExpr(tree: global.Tree): AST.Expr = tree match {
    case global.Block(stats, expr) =>
      AST.Block(stats.map(parseTerm), parseTerm(expr))
    case global.Apply(fun, args) =>
      AST.Apply(parseExpr(fun), args.map(parseExpr))
    case global.Literal(const: global.Constant) =>
      AST.Literal(const.value)
    case global.Ident(name) =>
      AST.LocalRef(name.toString)
    case global.Select(q, name) =>
      AST.MemberRef(parseExpr(q), name.toString)
    case global.Super(qual, mix) =>
      AST.Super(parseExpr(qual), mix.toString)
    case global.This(qual) =>
      AST.This(qual.toString)
    case unk =>
      unknown("Expr", unk)
  }

  sealed abstract class AST
  object AST {
    sealed abstract class Term extends AST
    sealed abstract class Expr extends Term

    // class, trait, or object
    case class CTO(name: String, body: Seq[AST]) extends Term
    case class DefDef(name: String, paramss: Seq[Seq[ValDef]], body: Term) extends Term
    case class ValDef(name: String, tpe: RT) extends Term

    case class Literal(value: Any) extends Expr
    case class LocalRef(name: String) extends Expr
    case class MemberRef(target: Expr, name: String) extends Expr
    case class Super(qual: Expr, mix: String) extends Expr
    case class This(qual: String) extends Expr

    case class Apply(ref: Term, args: Seq[Expr]) extends Expr

    case class Block(stats: Seq[Term], expr: Term) extends Expr
  }

  val annotationTpe = global.rootMirror.getRequiredClass("com.todesking.prety.refine").tpe
  def rtOf(sym: global.Symbol): RT = sym match {
    case s: global.TermSymbol =>
      val srcs =
        s.annotations.collect {
          case global.Annotation(tpe, sargs, jargs) if tpe <:< annotationTpe =>
            if (sargs.size != 1) throw new AssertionError(s"Expected exact 1 arguments: $sargs")
            val src = parseExpr(sargs(0)) match { case AST.Literal(src: String) => src }
        }
      if (srcs.size > 1) throw new RuntimeException(s"TODO: Nice error here")
      // TODO: Build sort here
      // TODO: default sort
      ???
    case unk =>
      throw new RuntimeException(s"Unsupported symbol: $sym(${sym.getClass.getName})")
  }

  private[this] val genSortVar = new GenSym(id => new Sort.Var(id))

  // "Refined Type"
  case class RT(tpe: Tpe, sort: Sort)
  sealed abstract class Tpe
  sealed abstract class Sort
  object Sort {
    class Var(id: Int) extends Sort
  }
}

class Pred {
}

class Preds(val primary: Pred, val named: Map[String, Pred])
object Preds {
  def parse(s: String): Preds = ???
}
