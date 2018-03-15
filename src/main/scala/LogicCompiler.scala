package com.todesking.prety

import org.sosy_lab.java_smt.api.SolverContext
import org.sosy_lab.java_smt.api.Formula
import org.sosy_lab.java_smt.api.NumeralFormula.IntegerFormula
import org.sosy_lab.java_smt.api.BooleanFormula

class LogicCompiler(solverContext: SolverContext) {
  private[this] var vars = Map.empty[String, Formula]
  private[this] implicit val ctx = solverContext
  import SMT.Syntax._

  def intVar(v: Logic.Var): IntegerFormula = {
    require(v.tpe == Logic.TInt)
    (vars.get(v.varName) getOrElse {
      ctx.intVar(v.varName)
    }).asInstanceOf[IntegerFormula]
  }
  def booleanVar(v: Logic.Var): BooleanFormula = {
    require(v.tpe == Logic.TBool)
    (vars.get(v.varName) getOrElse {
      ctx.intVar(v.varName)
    }).asInstanceOf[BooleanFormula]
  }
  def anyVar(v: Logic.Var): Formula = v.tpe match {
    case Logic.TInt => intVar(v)
    case Logic.TBool => booleanVar(v)
  }

  def compileInteger(l: Logic): IntegerFormula = l match {
    case Logic.IntValue(v) =>
      ctx.lit(v)
    case v @ Logic.Var(_, Logic.TInt, name) =>
      intVar(v)
    case unk =>
      throw new RuntimeException(s"SMT-I: $unk")
  }
  def compileBoolean(l: Logic): BooleanFormula = l match {
    case Logic.BoolValue(v) =>
      v
    case Logic.Eq(l, r) =>
      compileInteger(l) === compileInteger(r)
    case Logic.Gt(l, r) =>
      compileInteger(l) > compileInteger(r)
    case Logic.Lt(l, r) =>
      compileInteger(l) < compileInteger(r)
    case Logic.Implie(l, r) =>
      compileBoolean(l) --> compileBoolean(r)
    case Logic.And(conds) =>
      ctx.getFormulaManager.getBooleanFormulaManager.and(conds.map { c => compileBoolean(c) }: _*)
    case v @ Logic.Var(_, Logic.TBool, name) =>
      booleanVar(v)
    case Logic.Not(l) =>
      !compileBoolean(l)
    case Logic.Forall(vars, expr) =>
      ctx.forall(vars.toSeq.map(anyVar), compileBoolean(expr))
    case unk =>
      throw new RuntimeException(s"SMT-B: $unk")
  }
}
