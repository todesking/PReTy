package com.todesking.prety.universe

import com.todesking.prety.util.PP

trait ASTs { self: ForeignTypes with Values =>
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
      def value: Value
    }
    case class Block(value: Value, statements: Seq[InImpl], expr: Expr) extends Expr {
      override def pretty = PP(
        s"Block $value",
        PP.indent(statements.map(_.pretty) :+ expr.pretty))
    }
    case class This(value: Value) extends Expr {
      override def pretty = "This"
    }
    case class Apply(
      self: Expr,
      sym: DefSym,
      value: Value,
      argss: Seq[Seq[Expr]]) extends Expr {
      override def pretty = PP(
        s"Apply($sym) $value",
        PP.indent(self.pretty),
        PP.indent(argss.flatten.map(_.pretty)))
    }
    // TODO: where is `self`?
    case class LocalRef(
      sym: DefSym,
      value: Value) extends Expr {
      override def pretty = s"LocalRef($sym)"
    }
    case class Super(value: Value) extends Expr {
      override def pretty = s"Super $value"
    }
    sealed abstract class Literal extends Expr {
      val lit: Any
      override def pretty = s"Lit($lit) $value"
    }
    case class IntLiteral(value: Value, lit: Int) extends Literal {
    }
    case class BooleanLiteral(value: Value, lit: Boolean) extends Literal {
    }
    case class UnitLiteral(value: Value) extends Literal {
      override val lit = ()
    }

    case class If(value: Value, cond: Expr, thenp: Expr, elsep: Expr) extends Expr {
      override def pretty = PP(
        s"if $value",
        PP.indent(cond.pretty),
        "then",
        PP.indent(thenp.pretty),
        "else",
        PP.indent(elsep.pretty),
      )
    }

    case class FunDef(
      sym: DefSym,
      body: Option[Expr]) extends Term {
      override def pretty = PP(
        s"FunDef($sym)",
        PP.indent(body.map(_.pretty) getOrElse Seq()))
    }

    case class ValDef(
      sym: DefSym,
      body: Option[Expr]) extends Term {
      override def pretty = PP(
        s"ValDef($sym)",
        PP.indent(body.map(_.pretty) getOrElse Seq()))
    }
  }
}
