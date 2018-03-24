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
      val x = ctx.intVar(v.varName)
      this.vars = vars + (v.varName -> x)
      x
    }).asInstanceOf[IntegerFormula]
  }
  def booleanVar(v: Logic.BVar): BooleanFormula = {
    (vars.get(v.varName) getOrElse {
      val x = ctx.booleanVar(v.varName)
      this.vars = vars + (v.varName -> x)
      x
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
    case Logic.Plus(l, r) =>
      compileInteger(l) + compileInteger(r)
    case Logic.Div(l, r) =>
      compileInteger(l) / compileInteger(r)
    case Logic.Mul(l, r) =>
      compileInteger(l) * compileInteger(r)
    case unk =>
      throw new RuntimeException(s"SMT-I: $unk")
  }
  def compileBoolean(l: Logic): BooleanFormula = l match {
    case Logic.BValue(v) =>
      v
    case Logic.IEq(l, r) =>
      compileInteger(l) === compileInteger(r)
    case Logic.BEq(l, r) =>
      compileBoolean(l) === compileBoolean(r)
    case Logic.Gt(l, r) =>
      compileInteger(l) > compileInteger(r)
    case Logic.Ge(l, r) =>
      compileInteger(l) >= compileInteger(r)
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
      // TODO: ensure other variables not exists
      val ivars = vars.collect { case i @ Logic.IVar(_, _) => i }
      val bvars = vars.collect { case b @ Logic.BVar(_, _) => b }

      // java-smt's forall accepts only int vars??
      // So convert boolean var reference `b` to `bi == 1` where `bi = fresh_int_var()`
      val bis: Map[Logic.BVar, (Logic.IVar, Logic.LBool)] = bvars.map { bv =>
        val bivar = Logic.IVar(bv.id, bv.name)
        bv -> (bivar -> Logic.IEq(bivar, Logic.IValue(1)))
      }.toMap
      val sub = bis.map { case (k, v) => (k: Logic) -> v._2 }
      val body = expr.substitute(sub)
      val bodyLogic = compileBoolean(body)
      val newVars = ivars ++ bis.values.map(_._1)
      val smtVars = newVars.toSeq.map(intVar)
      ctx.forall(smtVars, bodyLogic)
    case unk =>
      throw new RuntimeException(s"SMT-B: $unk")
  }
}
