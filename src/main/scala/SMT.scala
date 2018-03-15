package com.todesking.prety

import org.sosy_lab.java_smt.api.SolverContext
import org.sosy_lab.java_smt.api.ProverEnvironment
import org.sosy_lab.common.ShutdownManager

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
  import org.sosy_lab.common.configuration.Configuration
  import org.sosy_lab.common.log.BasicLogManager
  import org.sosy_lab.java_smt.SolverContextFactory

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
    import org.sosy_lab.java_smt.api.NumeralFormula
    import NumeralFormula.IntegerFormula
    import org.sosy_lab.java_smt.api.BooleanFormula

    import scala.language.implicitConversions

    implicit def IntToFormula(i: Int)(implicit ctx: SolverContext) =
      ctx.lit(i)

    implicit class ContextOps(self: SolverContext) {
      private[this] def fm = self.getFormulaManager
      def intVar(name: String) =
        fm.getIntegerFormulaManager().makeVariable(name)
      def booleanVar(name: String) =
        fm.getBooleanFormulaManager().makeVariable(name)
      def lit(v: Int) =
        fm.getIntegerFormulaManager().makeNumber(v.toLong)
    }

    implicit class NumeralFormulaOps(self: IntegerFormula)(implicit ctx: SolverContext) {
      private[this] def ifm = ctx.getFormulaManager.getIntegerFormulaManager
      def +(rhs: IntegerFormula) = ifm.add(self, rhs)
      def >(rhs: IntegerFormula) = ifm.greaterThan(self, rhs)
      def <(rhs: IntegerFormula) = ifm.lessThan(self, rhs)
      def ===(rhs: IntegerFormula) = ifm.equal(self, rhs)
    }

    implicit class BooleanFormulaOps(self: BooleanFormula)(implicit ctx: SolverContext) {
      private[this] def fm = ctx.getFormulaManager.getBooleanFormulaManager
      def unary_!(): BooleanFormula = fm.not(self)
      def &&(rhs: BooleanFormula) = fm.and(self, rhs)
      def -->(rhs: BooleanFormula) =
        fm.implication(self, rhs)
    }
    implicit def BooleanToFormula(b: Boolean)(implicit ctx: SolverContext): BooleanFormula =
      ctx.getFormulaManager.getBooleanFormulaManager.makeBoolean(b)
  }
}
