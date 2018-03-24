package com.todesking.prety

import org.sosy_lab.common.ShutdownManager
import org.sosy_lab.common.log.BasicLogManager
import org.sosy_lab.common.configuration.Configuration

import org.sosy_lab.java_smt.api.SolverContext
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.api.ProverEnvironment

import org.sosy_lab.java_smt.api.Formula
import org.sosy_lab.java_smt.api.BooleanFormula
import org.sosy_lab.java_smt.api.NumeralFormula
import NumeralFormula.IntegerFormula
import org.sosy_lab.java_smt.api.BitvectorFormula
import org.sosy_lab.java_smt.api.NumeralFormulaManager
import org.sosy_lab.java_smt.api.IntegerFormulaManager

import scala.language.implicitConversions

import scala.collection.JavaConverters._

class SMT(val ctx: SolverContext, val shutdownManager: ShutdownManager) {
  def shutdown(): Unit =
    shutdownManager.requestShutdown("bye")

  def withProver[A](options: SolverContext.ProverOptions*)(f: ProverEnvironment => A): A = {
    val env = ctx.newProverEnvironment(options: _*)
    try {
      f(env)
    } finally {
      env.close()
    }
  }
}

object SMT {
  val config = Configuration.builder.build()
  val logger = BasicLogManager.create(config)

  def newContext() = {
    val shutdown = ShutdownManager.create()
    val ctx =
      SolverContextFactory.createSolverContext(
        config,
        logger,
        shutdown.getNotifier(),
        SolverContextFactory.Solvers.PRINCESS)
    new SMT(ctx, shutdown)
  }

  object Syntax {

    implicit def IntToFormula(i: Int)(implicit ctx: SolverContext) =
      ctx.lit(i)

    implicit class ContextOps(self: SolverContext) {
      private[this] def fm = self.getFormulaManager
      def intVar(name: String) =
        fm.getIntegerFormulaManager.makeVariable(name)
      def int32Var(name: String) =
        fm.getBitvectorFormulaManager.makeVariable(32, name)
      def booleanVar(name: String) =
        fm.getBooleanFormulaManager.makeVariable(name)
      def lit(v: Int) =
        fm.getIntegerFormulaManager.makeNumber(v.toLong)
      def lit(v: Boolean) =
        fm.getBooleanFormulaManager.makeBoolean(v)
      def forall(vars: Seq[Formula], expr: BooleanFormula): BooleanFormula =
        self.getFormulaManager.getQuantifiedFormulaManager.forall(vars.asJava, expr)
    }

    abstract class AbstractNumeralOps[A <: NumeralFormula, M <: NumeralFormulaManager[A, A]] {
      protected val fm: M
      protected val self: A
      def +(rhs: A): A = fm.add(self, rhs)
      def /(rhs: A): A = fm.divide(self, rhs)
      def *(rhs: A): A = fm.multiply(self, rhs)
      def >(rhs: A): BooleanFormula = fm.greaterThan(self, rhs)
      def >=(rhs: A): BooleanFormula = fm.greaterOrEquals(self, rhs)
      def <(rhs: A): BooleanFormula = fm.lessThan(self, rhs)
      def ===(rhs: A): BooleanFormula = fm.equal(self, rhs)
    }

    implicit class IntegerFormulaOps(override val self: IntegerFormula)(implicit ctx: SolverContext)
      extends AbstractNumeralOps[IntegerFormula, IntegerFormulaManager] {
      override val fm = ctx.getFormulaManager.getIntegerFormulaManager
    }

    implicit class BitvectorFormulaOps(self: BitvectorFormula)(implicit ctx: SolverContext) {
      private[this] val fm = ctx.getFormulaManager.getBitvectorFormulaManager
      type A = BitvectorFormula
      val signed = true
      def +(rhs: A): A = fm.add(self, rhs)
      def /(rhs: A): A = fm.divide(self, rhs, signed)
      def *(rhs: A): A = fm.multiply(self, rhs)
      def >(rhs: A): BooleanFormula = fm.greaterThan(self, rhs, signed)
      def >=(rhs: A): BooleanFormula = fm.greaterOrEquals(self, rhs, signed)
      def <(rhs: A): BooleanFormula = fm.lessThan(self, rhs, signed)
      def ===(rhs: A): BooleanFormula = fm.equal(self, rhs)
    }

    implicit class BooleanFormulaOps(self: BooleanFormula)(implicit ctx: SolverContext) {
      private[this] def fm = ctx.getFormulaManager.getBooleanFormulaManager
      def unary_!(): BooleanFormula = fm.not(self)
      def ===(rhs: BooleanFormula) = fm.equivalence(self, rhs)
      def &&(rhs: BooleanFormula) = fm.and(self, rhs)
      def -->(rhs: BooleanFormula) =
        fm.implication(self, rhs)
    }
    implicit def BooleanToFormula(b: Boolean)(implicit ctx: SolverContext): BooleanFormula =
      ctx.getFormulaManager.getBooleanFormulaManager.makeBoolean(b)
  }
}
