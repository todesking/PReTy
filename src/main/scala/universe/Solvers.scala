package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Solvers { self: ForeignTypes with Queries with Values with Graphs with Constraints with Conflicts with Preds with Envs with Preds with Props with Worlds with Debugging =>
  object Solver {
    def solve(g: Graph): Seq[Conflict] = {
      val (nontrivials, trivialConflicts) = solveTrivial(g.groundConstraints)
      val nontrivialConflicts = solveSMT(nontrivials, g.binding)
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

    def solveSMT(constraints: Seq[GroundConstraint], binding: Map[Value, Pred]): Seq[Conflict] = {
      val (ls, cs) =
        constraints.map(compileConstraint(_, binding))
          .foldLeft((Seq.empty[LogicConstraint], Seq.empty[Conflict])) {
            case ((al, ac), (l, c)) =>
              (al :+ l, ac ++ c)
          }
      cs ++ runSMT(ls)
    }

    private[this] def compileConstraint(c: GroundConstraint, binding: Map[Value, Pred]): (LogicConstraint, Seq[Conflict]) = {
      val xs =
        propConstraints(c).map {
          case (t, l, r) =>
            globalEnv.findWorld(t).solveConstraint(c.env, binding, l, r)
        }
      val logics = xs.flatMap(_._1)
      val conflicts = xs.flatMap(_._2)
      (LogicConstraint(c, Logic.and(logics)), conflicts)
    }

    private[this] def propConstraints(c: GroundConstraint): Seq[(TypeSym, PropPred, PropPred)] = {
      val l = c.lhs.cast(c.rhs.tpe)
      val r = c.rhs
      val keys = l.definedProps.keySet ++ r.definedProps.keySet
      keys.toSeq.map { k =>
        (k.tpe, l.prop(k), r.prop(k))
      }
    }

    private[this] def runSMT(constraints: Seq[LogicConstraint]): Seq[Conflict] = {
      dprint("SMT Logic:")
      dprint(constraints.map { x => "  " + x.toString }.mkString("\n"))

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
        case Logic.Lt(l, r) =>
          smtI(l) < smtI(r)
        case Logic.Implie(l, r) =>
          smtB(l) --> smtB(r)
        case Logic.And(conds) =>
          ctx.getFormulaManager.getBooleanFormulaManager.and(conds.map { c => smtB(c) }: _*)
        case Logic.Var(_, Logic.TBool) =>
          ctx.booleanVar(l.toString)
        case Logic.Not(l) =>
          !smtB(l)
        case unk =>
          throw new RuntimeException(s"SMT-B: $unk")
      }
      def fvars(l: Logic): Set[Logic.Var] = l match {
        case Logic.Eq(l, r) =>
          fvars(l) ++ fvars(r)
        case Logic.Gt(l, r) =>
          fvars(l) ++ fvars(r)
        case Logic.Lt(l, r) =>
          fvars(l) ++ fvars(r)
        case Logic.Implie(l, r) =>
          fvars(l) ++ fvars(r)
        case Logic.And(xs) =>
          xs.flatMap(fvars).toSet
        case v @ Logic.Var(_, _) =>
          Set(v)
        case _ =>
          Set()
      }

      val conflicts =
        withResource(ctx.newProverEnvironment()) { prover =>
          dprint("Compiled SMT:")
          constraints.foreach { c =>
            val l = c.logic
            val fvs = fvars(l)
            dprint(s"  forall ${fvs.mkString(", ")}. $l")
          }

          constraints.flatMap { c =>
            val l = c.logic
            import scala.collection.JavaConverters._
            val fvs = fvars(l)
            val smt = smtB(l)
            val quantified =
              ctx.getFormulaManager.getQuantifiedFormulaManager.forall(
                fvs.toSeq.map {
                  case v @ Logic.Var(_, Logic.TInt) =>
                    ctx.getFormulaManager.getIntegerFormulaManager.makeVariable(v.toString)
                }.asJava,
                smt)
            prover.push(quantified)
            val unsat = prover.isUnsat()
            prover.pop()
            if (unsat) {
              dprint(s"Unsat: $l; $quantified")
              Some(Conflict(c.constraint))
            } else {
              None
            }
          }
        }
      SMT.shutdown.requestShutdown("die")
      conflicts
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
        SolverContextFactory.Solvers.PRINCESS)
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
}
