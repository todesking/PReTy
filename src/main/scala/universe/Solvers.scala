package com.todesking.prety.universe

import com.todesking.prety.Logic
import com.todesking.prety.SMT

trait Solvers { self: ForeignTypes with Queries with Values with ValueRepos with Graphs with Constraints with Conflicts with Preds with Envs with Preds with Props with Worlds with Debugging =>
  class Solver(world: World) {
    private[this] val valueRepo = world.values

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
          case (key, l, r) =>
            world.findProp(key.typeFor(c.focus.tpe)).solveConstraint(c.focus, key, c.env, binding, l, r)
        }
      val logics = xs.flatMap(_._1)
      val conflicts = xs.flatMap(_._2)
      (LogicConstraint(c, Logic.and(logics)), conflicts)
    }

    private[this] def propConstraints(c: GroundConstraint): Seq[(PropKey, PropPred, PropPred)] = {
      val l = c.lhs.cast(c.rhs.tpe)
      val r = c.rhs
      val keys = l.definedProps.keySet ++ r.definedProps.keySet
      keys.toSeq.map { k =>
        (k, l.prop(k), r.prop(k))
      }
    }

    private[this] def runSMT(constraints: Seq[LogicConstraint]): Seq[Conflict] = {
      def pos(v: Value) = valueRepo.getPos(v) match {
        case Some(p) =>
          s"${query.lineNum(p)}:${query.columnNum(p)}"
        case None =>
          s"???"
      }
      dprint("SMT Logic:")
      constraints.foreach { c =>
        def valueString(v: Value) = s"$v: ${c.constraint.binding(v)}"
        def show(v: Value): String =
          s"${pos(v)} ${valueString(v)}"

        dprint(show(c.constraint.focus))
        c.constraint.env.values.foreach { v =>
          dprint("  ENV:", show(v))
        }
        c.constraint.env.conds.foreach { v =>
          dprint("  COND:", show(v))
        }
        c.constraint.env.unconds.foreach { v =>
          dprint("  UNCOND:", show(v))
        }
        dprint("  =>", c.constraint)
        dprint("  =>", c.logic)
      }

      val smt = SMT.newContext()
      implicit val ctx = smt.ctx
      import SMT.Syntax._

      import org.sosy_lab.java_smt.api.NumeralFormula.IntegerFormula
      import org.sosy_lab.java_smt.api.BooleanFormula
      def smtI(l: Logic): IntegerFormula = l match {
        case Logic.IntValue(v) =>
          ctx.lit(v)
        case v @ Logic.Var(_, Logic.TInt, name) =>
          ctx.intVar(v.varName)
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
        case v @ Logic.Var(_, Logic.TBool, name) =>
          ctx.booleanVar(v.varName)
        case Logic.Not(l) =>
          !smtB(l)
        case unk =>
          throw new RuntimeException(s"SMT-B: $unk")
      }

      val conflicts =
        smt.withProver() { prover =>
          dprint("Compiled SMT:")
          constraints.foreach { c =>
            val l = c.logic
            val fvs = l.vars
            dprint(s"  forall ${fvs.mkString(", ")}. $l")
          }

          constraints.flatMap { c =>
            val l = c.logic
            import scala.collection.JavaConverters._
            val fvs = l.vars
            val smt = smtB(l)
            val quantified =
              ctx.getFormulaManager.getQuantifiedFormulaManager.forall(
                fvs.toSeq.map {
                  case v @ Logic.Var(_, Logic.TInt, name) =>
                    ctx.getFormulaManager.getIntegerFormulaManager.makeVariable(v.varName)
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
      smt.shutdown()
      conflicts
    }
    private[this] def withResource[A <: AutoCloseable, B](r: A)(f: A => B): B =
      try {
        f(r)
      } finally {
        r.close()
      }
  }
}
