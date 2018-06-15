package com.todesking.prety.universe

import com.todesking.prety.Logic
import com.todesking.prety.SMT
import com.todesking.prety.LogicCompiler

trait Solvers { self: ForeignTypes with Values with Graphs with Constraints with Conflicts with Preds with Envs with Preds with Props with Exprs with Worlds with Debugging =>
  class Solver(world: World) {
    private[this] val valueRepo = world.values

    def solve(g: Graph): Seq[Conflict] = {
      val (cs, trivialConflicts) = solveTrivial(g.groundConstraints.map(simplify))
      val nontrivialConflicts = solveSMT(cs, g.binding)
      trivialConflicts ++ nontrivialConflicts
    }

    private[this] def simplify(g: GroundConstraint): GroundConstraint = g

    private[this] def solveTrivial(constraints: Seq[GroundConstraint]): (Seq[GroundConstraint], Seq[Conflict]) = {
      val (cs, cfs) =
        splitMap(constraints) { c =>
          if (c.lhs == c.rhs) Some(None)
          else None
        }
      (cs, cfs.flatten)
    }
    private[this] def splitMap[A, B](xs: Seq[A])(f: A => Option[B]): (Seq[A], Seq[B]) =
      xs.foldLeft((Seq.empty[A], Seq.empty[B])) {
        case ((aa, ab), x) =>
          f(x).fold((aa :+ x, ab)) { b => (aa, ab :+ b) }
      }

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
          case (path, l, r) =>
            solveTrivial(l, r) getOrElse {
              world
                .findProp(path.lastOption.map(_.tpe) getOrElse c.tpe)
                .solveConstraint(c.focus, path, c.env, binding, l, r)
            }
        }
      val logics = xs.flatMap(_._1)
      val conflicts = xs.flatMap(_._2)
      (LogicConstraint(c, Logic.and(logics).universalQuantifiedForm), conflicts)
    }

    private[this] def solveTrivial(l: Expr, r: Expr): Option[(Seq[Logic.LBool], Seq[Conflict])] = (l, r) match {
      case (CoreExpr.True, CoreExpr.True) => Some((Seq(), Seq()))
      case _ => None
    }

    private[this] def propConstraints(c: GroundConstraint): Seq[(Seq[PropKey], Expr, Expr)] = {
      def gather(l: Pred, r: Pred, path: Seq[PropKey]): Seq[(Seq[PropKey], Expr, Expr)] = {
        (path, l.self, r.self) +: r.propKeys.toSeq.flatMap { k =>
          if (l.customized(k) || r.customized(k))
            gather(l.prop(k), r.prop(k), path :+ k)
          else
            Seq()
        }
      }
      val l = c.lhs.cast(c.rhs.tpe)
      val r = c.rhs
      gather(l, r, Seq())
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
            val compiled = compiler.compileBoolean(c.logic)
            prover.push(compiled)
            val unsat = prover.isUnsat()
            prover.pop()
            if (unsat) {
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
