package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Solvers { self: ForeignTypes with Graphs with Constraints with Conflicts with Preds with Envs with Preds with Props with Worlds =>
  object Solver {
    def solve(g: Graph): Seq[Conflict] = {
      val (nontrivials, trivialConflicts) = solveTrivial(g.groundConstraints)
      val nontrivialConflicts = solveSMT(nontrivials)
      trivialConflicts ++ nontrivialConflicts
    }
    def solveTrivial(cs: Seq[GroundConstraint]): (Seq[GroundConstraint], Seq[Conflict]) = {
      cs.foldLeft(
        (Seq.empty[GroundConstraint], Seq.empty[Conflict])) {
          case ((ct, cf), c) =>
            (simplify(c.lhs), simplify(c.rhs)) match {
              case (l, r) if l == r =>
                (ct, cf)
              case (_, Pred.True) =>
                (ct, cf)
              case _ =>
                (ct :+ c, cf)
            }
        }
    }

    def simplify(p: Pred): Pred = p

    def solveSMT(constraints: Seq[GroundConstraint]): Seq[Conflict] = {
      val (ls, cs) = constraints.flatMap { c =>
        propConstraints(c).map {
          case (t, l, r) =>
            globalEnv.findWorld(t).solveConstraint(l, r)
        }
      }.foldLeft((Seq.empty[Logic], Seq.empty[Conflict])) {
        case ((al, ac), (l, c)) =>
          (al ++ l, ac ++ c)
      }
      cs ++ runSMT(ls)
    }
    private[this] def propConstraints(c: GroundConstraint): Seq[(TypeSym, PropPred, PropPred)] = {
      val l = c.lhs.upcast(c.rhs.tpe)
      val r = c.rhs
      val keys = l.definedProps.keySet ++ r.definedProps.keySet
      keys.toSeq.map { k =>
        (k.tpe, l.prop(k), r.prop(k))
      }
    }

    private[this] def runSMT(logics: Seq[Logic]): Seq[Conflict] = {
      println("SMT Logic:")
      println(logics.map { x => "  " + x.toString }.mkString("\n"))

      implicit val ctx = SMT.newContext()
      import SMTSyntax._

      import org.sosy_lab.java_smt.api.NumeralFormula.IntegerFormula
      import org.sosy_lab.java_smt.api.BooleanFormula
      def smtI(l: Logic): IntegerFormula = l match {
        case Logic.IntValue(v) =>
          ctx.lit(v)
        case v @ Logic.Var(_, Logic.TInt) =>
          ctx.intVar(v.toString)
        case unk =>
          throw new RuntimeException(s"SMT-I: $unk")
      }
      def smtB(l: Logic): BooleanFormula = l match {
        case Logic.BoolValue(v) =>
          v
        case Logic.Eq(l, r) =>
          smtI(l) === smtI(r)
        case Logic.Gt(l, r) =>
          smtI(l) > smtI(r)
        case Logic.Implie(l, r) =>
          smtB(l) --> smtB(r)
        case unk =>
          throw new RuntimeException(s"SMT-B: $unk")
      }
      import org.sosy_lab.java_smt.api.SolverContext.ProverOptions

      def fvars(l: Logic): Set[Logic.Var] = l match {
        case Logic.Eq(l, r) =>
          fvars(l) ++ fvars(r)
        case Logic.Gt(l, r) =>
          fvars(l) ++ fvars(r)
        case Logic.Implie(l, r) =>
          fvars(l) ++ fvars(r)
        case v @ Logic.Var(_, _) =>
          Set(v)
        case _ =>
          Set()
      }

      withResource(ctx.newProverEnvironment(ProverOptions.GENERATE_UNSAT_CORE)) { prover =>
        println("Compiled SMT:")
        logics.foreach { l =>
          val fvs = fvars(l)
          val smt = smtB(l)
          println(s"  forall ${fvs.mkString(", ")}. $l")
          import scala.collection.JavaConverters._
          val quantified =
            ctx.getFormulaManager.getQuantifiedFormulaManager.forall(
              fvs.toSeq.map {
                case v @ Logic.Var(_, Logic.TInt) =>
                  ctx.getFormulaManager.getIntegerFormulaManager.makeVariable(v.toString)
              }.asJava,
              smt)
          prover.addConstraint(quantified)
        }
        val unsat = prover.isUnsat()
        println(unsat)
        if (unsat) {
          println(prover.getUnsatCore())
        }
      }
      SMT.shutdown.requestShutdown("die")
      Seq()
    }
    private[this] def withResource[A <: AutoCloseable, B](r: A)(f: A => B): B =
      try {
        f(r)
      } finally {
        r.close()
      }

    object SMT {
      import org.sosy_lab.common.{ ShutdownManager }
      import org.sosy_lab.common.configuration.Configuration
      import org.sosy_lab.common.log.BasicLogManager
      import org.sosy_lab.java_smt.SolverContextFactory

      val config = Configuration.builder.build()
      val logger = BasicLogManager.create(config)
      val shutdown = ShutdownManager.create()

      def newContext() = SolverContextFactory.createSolverContext(
        config,
        logger,
        shutdown.getNotifier(),
        SolverContextFactory.Solvers.SMTINTERPOL)
    }
    object SMTSyntax {
      import org.sosy_lab.java_smt.api.SolverContext
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
        def lit(v: Int) =
          fm.getIntegerFormulaManager().makeNumber(v.toLong)
      }

      implicit class NumeralFormulaOps(self: IntegerFormula)(implicit ctx: SolverContext) {
        private[this] def ifm = ctx.getFormulaManager.getIntegerFormulaManager
        def +(rhs: IntegerFormula) = ifm.add(self, rhs)
        def >(rhs: IntegerFormula) = ifm.greaterThan(self, rhs)
        def ===(rhs: IntegerFormula) = ifm.equal(self, rhs)
      }

      implicit class BooleanFormulaOps(self: BooleanFormula)(implicit ctx: SolverContext) {
        private[this] def fm = ctx.getFormulaManager.getBooleanFormulaManager
        def &&(rhs: BooleanFormula) = fm.and(self, rhs)
        def -->(rhs: BooleanFormula) =
          fm.implication(self, rhs)
      }
      implicit def BooleanToFormula(b: Boolean)(implicit ctx: SolverContext): BooleanFormula =
        ctx.getFormulaManager.getBooleanFormulaManager.makeBoolean(b)
    }
  }
}
