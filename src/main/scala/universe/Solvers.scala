package com.todesking.prety.universe

import com.todesking.prety.Logic
import com.todesking.prety.SMT
import com.todesking.prety.LogicCompiler

trait Solvers { self: ForeignTypes with Values with Graphs with Constraints with Conflicts with Preds with Envs with Preds with Props with Worlds with Debugging =>
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

    def solveSMT(constraints: Seq[GroundConstraint], binding: Map[Value.Naked, Pred]): Seq[Conflict] = {
      val (ls, cs) =
        constraints.map(compileConstraint(_, binding))
          .foldLeft((Seq.empty[LogicConstraint], Seq.empty[Conflict])) {
            case ((al, ac), (l, c)) =>
              (al :+ l, ac ++ c)
          }
      cs ++ runSMT(ls)
    }

    private[this] def compileConstraint(c: GroundConstraint, binding: Map[Value.Naked, Pred]): (LogicConstraint, Seq[Conflict]) = {
      val xs =
        propConstraints(c).map {
          case (key, l, r) =>
            world.findProp(key.typeFor(c.focus.tpe)).solveConstraint(c.focus, key, c.env, binding, l, r)
        }
      val logics = xs.flatMap(_._1)
      val conflicts = xs.flatMap(_._2)
      (LogicConstraint(c, Logic.and(logics).universalQuantifiedForm), conflicts)
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
      val smt = SMT.newContext()
      val compiler = new LogicCompiler(smt.ctx)

      dprint("SMT Logic:")
      constraints.foreach { c =>
        def valueString(v: Value) = s"$v: ${c.constraint.binding(v.naked)}"
        def show(v: Value): String =
          s"${pos(v)} ${valueString(v)}"

        dprint(show(c.constraint.focus))
        c.constraint.env.conds.foreach { v =>
          dprint("  COND:", show(v))
        }
        c.constraint.env.unconds.foreach { v =>
          dprint("  UNCOND:", show(v))
        }
        dprint("  =>", c.constraint)
        dprint("  =>", c.logic)
        dprint("  =>", compiler.compileBoolean(c.logic))
      }

      val conflicts =
        smt.withProver() { prover =>
          constraints.flatMap { c =>
            def valueString(v: Value) = s"$v: ${c.constraint.binding(v.naked)}"
            def show(v: Value): String =
              s"${pos(v)} ${valueString(v)}"
            val compiled = compiler.compileBoolean(c.logic)
            dprint(s"Running ${show(c.constraint.focus)}")
            dprint(s"  Compiled: ${compiled}")
            prover.push(compiled)
            val unsat = prover.isUnsat()
            prover.pop()
            dprint(s"  => ${if (unsat) "UNSAT" else "SAT"}")
            if (unsat) {
              dprint(s"Unsat: ${c.logic}; $compiled")
              Some(Conflict(c.constraint))
            } else {
              None
            }
          }
        }
      smt.shutdown()
      conflicts
    }
  }
}
