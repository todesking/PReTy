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

    private[this] def runSMT(l: Seq[Logic]): Seq[Conflict] = {
      println("SMT Logic:")
      println(l.map { x => "  " + x.toString }.mkString("\n"))

      import org.sosy_lab.common.{ ShutdownManager }
      import org.sosy_lab.common.configuration.Configuration
      import org.sosy_lab.common.log.BasicLogManager
      import org.sosy_lab.java_smt.SolverContextFactory
      import org.sosy_lab.java_smt.api.SolverContext
      import SolverContext.ProverOptions

      val config = Configuration.builder.build()
      val logger = BasicLogManager.create(config)
      val shutdown = ShutdownManager.create()

      val ctx = SolverContextFactory.createSolverContext(
        config,
        logger,
        shutdown.getNotifier(),
        SolverContextFactory.Solvers.SMTINTERPOL)
      val fm = ctx.getFormulaManager()
      val ifm = fm.getIntegerFormulaManager()
      val bfm = fm.getBooleanFormulaManager()
      val i1 = ifm.makeVariable("i1")
      val i2 = ifm.makeVariable("i2")
      val constraint = bfm.and(
        ifm.equal(i1, i2),
        ifm.greaterThan(i1, i2))
      withResource(ctx.newProverEnvironment(ProverOptions.GENERATE_UNSAT_CORE)) { prover =>
        prover.addConstraint(constraint)
        println(constraint)
        val unsat = prover.isUnsat()
        println(unsat)
        if (unsat) {
          println(prover.getUnsatCore())
        }
      }
      shutdown.requestShutdown("die")
      Seq()
    }
    private[this] def withResource[A <: AutoCloseable, B](r: A)(f: A => B): B =
      try {
        f(r)
      } finally {
        r.close()
      }
  }
}
