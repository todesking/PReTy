package com.todesking.prety.scalac_plugin.universe

trait ASTs { self: ForeignTypes =>
  def toAST(t: Tree): Seq[AST.CTODef]

  case class Value(id: Int, name: String) {
    def tpe: TypeSym = ???
    override def toString = s"$name#$id"
  }
  object Value {
    private[this] val genSym = new GenSym()
    def fresh(name: String = ""): Value = Value(genSym(), name)
  }

  sealed abstract class AST
  object AST {
    sealed trait InTopLevel extends AST
    sealed trait InImpl extends AST

    case class CTODef(impl: Seq[InImpl]) extends AST with InTopLevel with InImpl

    sealed abstract class Term extends AST with InImpl
    sealed abstract class Expr extends Term {
      def tpe: TypeSym
      def value: Value
    }
    case class Block(tpe: TypeSym, value: Value, statements: Seq[InImpl], expr: Expr) extends Expr
    case class This(tpe: TypeSym, value: Value) extends Expr
    case class Apply(
      self: Expr,
      sym: DefSym,
      tpe: TypeSym,
      value: Value,
      argss: Seq[Seq[Expr]]) extends Expr
    // TODO: where is `self`?
    case class ValRef(
      sym: DefSym,
      tpe: TypeSym,
      value: Value) extends Expr
    case class Super(tpe: TypeSym, value: Value) extends Expr
    case class Select(tpe: TypeSym, value: Value, target: Expr, sym: DefSym) extends Expr
    sealed abstract class Literal extends Expr {
      val lit: Any
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
      body: Option[Expr]) extends Term
    case class ValDef(
      sym: DefSym,
      tpe: TypeSym,
      value: Value,
      body: Option[Expr]) extends Term
  }
}
