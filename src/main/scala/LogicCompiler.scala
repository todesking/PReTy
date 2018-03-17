package com.todesking.prety

import org.sosy_lab.java_smt.api.SolverContext
import org.sosy_lab.java_smt.api.Formula
import org.sosy_lab.java_smt.api.NumeralFormula.IntegerFormula
import org.sosy_lab.java_smt.api.BooleanFormula

class LogicCompiler(solverContext: SolverContext) {
  private[this] var vars = Map.empty[String, Formula]
  private[this] implicit val ctx = solverContext
  import SMT.Syntax._

  def intVar(v: Logic.IVar): IntegerFormula = {
    (vars.get(v.varName) getOrElse {
      ctx.intVar(v.varName)
    }).asInstanceOf[IntegerFormula]
  }
  def booleanVar(v: Logic.BVar): BooleanFormula = {
    (vars.get(v.varName) getOrElse {
      ctx.booleanVar(v.varName)
    }).asInstanceOf[BooleanFormula]
  }
  def anyVar(v: Logic.Var): Formula = v match {
    case b @ Logic.BVar(_, _) => booleanVar(b)
    case i @ Logic.IVar(_, _) => intVar(i)
  }

  def compileInteger(l: Logic): IntegerFormula = l match {
    case Logic.IValue(v) =>
      ctx.lit(v)
    case v @ Logic.IVar(_, _) =>
      intVar(v)
    case unk =>
      throw new RuntimeException(s"SMT-I: $unk")
  }
  def compileBoolean(l: Logic): BooleanFormula = l match {
    case Logic.BValue(v) =>
      v
    case Logic.IEq(l, r) =>
      compileInteger(l) === compileInteger(r)
    case Logic.Gt(l, r) =>
      compileInteger(l) > compileInteger(r)
    case Logic.Lt(l, r) =>
      compileInteger(l) < compileInteger(r)
    case Logic.Implie(l, r) =>
      compileBoolean(l) --> compileBoolean(r)
    case Logic.And(conds) =>
      ctx.getFormulaManager.getBooleanFormulaManager.and(conds.map { c => compileBoolean(c) }: _*)
    case v @ Logic.BVar(_, _) =>
      booleanVar(v)
    case Logic.Not(l) =>
      !compileBoolean(l)
    case Logic.Forall(vars, expr) =>
      ctx.forall(vars.toSeq.map(anyVar), compileBoolean(expr))
    case unk =>
      throw new RuntimeException(s"SMT-B: $unk")
  }
}
