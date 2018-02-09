package com.todesking.prety.scalac_plugin.universe

import scala.language.implicitConversions

trait ASTs { self: ForeignTypes =>
  def toAST(t: Tree): Seq[AST.CTODef]

  sealed abstract class PP {
    override def toString = toString(0)
    def toString(level: Int): String
    def isEmpty: Boolean
    def indent: PP = PP.Indent(Seq(this))
  }
  object PP {
    implicit def stringToPP(v: String) = Line(v)
    implicit def seqToPP(v: Seq[PP]) = Items(v)

    def indent(ps: PP*) = Indent(ps.filterNot(_.isEmpty))
    def apply(ps: PP*) = Items(ps.filterNot(_.isEmpty))

    case class Line(value: String) extends PP {
      override def isEmpty = false
      override def toString(level: Int) =
        "  " * level + value
    }
    case class Items(items: Seq[PP]) extends PP {
      override def isEmpty = items.isEmpty
      override def toString(level: Int) =
        items.map(_.toString(level)).mkString("\n")
    }
    case class Indent(items: Seq[PP]) extends PP {
      override def isEmpty = items.isEmpty
      override def toString(level: Int) =
        items.map(_.toString(level + 1)).mkString("\n")
    }
  }

  case class Value(id: Int, name: String) {
    def tpe: TypeSym = ???
    override def toString = s"$name#$id"
  }
  object Value {
    private[this] val genSym = new GenSym()
    def fresh(name: String = ""): Value = Value(genSym(), name)
  }

  sealed abstract class AST {
    def pretty: PP
  }
  object AST {
    sealed trait InTopLevel extends AST
    sealed trait InImpl extends AST

    case class CTODef(impl: Seq[InImpl]) extends AST with InTopLevel with InImpl {
      override def pretty = PP(
        "CTO",
        PP.indent(impl.map(_.pretty)))
    }

    sealed abstract class Term extends AST with InImpl
    sealed abstract class Expr extends Term {
      def tpe: TypeSym
      def value: Value
    }
    case class Block(tpe: TypeSym, value: Value, statements: Seq[InImpl], expr: Expr) extends Expr {
      override def pretty = PP(
        "Block",
        PP.indent(statements.map(_.pretty) :+ expr.pretty))
    }
    case class This(tpe: TypeSym, value: Value) extends Expr {
      override def pretty = "This"
    }
    case class Apply(
      self: Expr,
      sym: DefSym,
      tpe: TypeSym,
      value: Value,
      argss: Seq[Seq[Expr]]) extends Expr {
      override def pretty = PP(
        s"Apply($sym)",
        PP.indent(self.pretty),
        PP.indent(argss.flatten.map(_.pretty)))
    }
    // TODO: where is `self`?
    case class ValRef(
      sym: DefSym,
      tpe: TypeSym,
      value: Value) extends Expr {
      override def pretty = s"ValRef($sym)"
    }
    case class Super(tpe: TypeSym, value: Value) extends Expr {
      override def pretty = s"Super"
    }
    sealed abstract class Literal extends Expr {
      val lit: Any
      override def pretty = s"Lit($lit)"
    }
    case class IntLiteral(value: Value, lit: Int) extends Literal {
      override def tpe = ???
    }
    case class UnitLiteral(value: Value) extends Literal {
      override val lit = ()
      override def tpe = ???
    }

    case class FunDef(
      sym: DefSym,
      tpe: TypeSym,
      body: Option[Expr]) extends Term {
      override def pretty = PP(
        s"FunDef($sym)",
        PP.indent(body.map(_.pretty) getOrElse Seq()))
    }

    case class ValDef(
      sym: DefSym,
      tpe: TypeSym,
      value: Value,
      body: Option[Expr]) extends Term {
      override def pretty = PP(
        s"ValDef($sym)",
        PP.indent(body.map(_.pretty) getOrElse Seq()))
    }
  }
}
