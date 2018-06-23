package com.todesking.prety.universe

import com.todesking.prety.Logic
import com.todesking.prety.SMT
import com.todesking.prety.LogicCompiler

trait Solvers { self: ForeignTypes with Values with Graphs with Constraints with Conflicts with Preds with Envs with Preds with Props with Exprs with Worlds with Debugging =>
  class Solver(world: World) {
    private[this] val valueRepo = world.values

    def solve(g: Graph): Seq[Conflict] =
      solveSMT(g.groundConstraints, g.binding)

    def solveSMT(constraints: Seq[GroundConstraint], binding: Map[Value.Naked, Pred]): Seq[Conflict] = {
      val (ls, cs) =
        constraints.map(compileConstraint(_, binding))
          .foldLeft((Seq.empty[LogicConstraint], Seq.empty[Conflict])) {
            case ((al, ac), (l, c)) =>
              (al :+ l, ac ++ c)
          }
      cs ++ runSMT(ls, binding)
    }

    private[this] def compileConstraint(c: GroundConstraint, binding: Map[Value.Naked, Pred]): (LogicConstraint, Seq[Conflict]) = {
      val pcs = propConstraints(c)
      dprint("Compilation of", pos(c.focus), c.focus.shortString, c)
      pcs.foreach {
        case (path, l, r) =>
          dprint("  ", path.map(_.name).mkString("/", "/", ""), l, "<=", r)
      }
      val xs =
        pcs.map {
          case (path, l, r) =>
            solveTrivial(l, r) getOrElse {
              world
                .findProp(path.lastOption.map(_.tpe) getOrElse c.tpe)
                .solveConstraint(c.focus, path, c.env, binding, l, r)
            }
        }
      val env = binding.map {
        case (v, p) =>
          compileTrivial(v, p.self) getOrElse {
            world.findProp(v.tpe).toLogic(p.self, v)
          }
      }.toSeq
      val logics = xs.flatMap(_._1)
      val conflicts = xs.flatMap(_._2)
      (LogicConstraint(c, Logic.and(env) & Logic.and(logics).universalQuantifiedForm), conflicts)
    }

    private[this] def compileTrivial(v: Value.Naked, e: Expr): Option[Logic.LBool] = simplify(e) match {
      case CoreExpr.BOOL_Lit(b) => Some(Logic.BValue(b))
      case _ => None
    }

    private[this] def solveTrivial(l: Expr, r: Expr): Option[(Seq[Logic.LBool], Seq[Conflict])] = (simplify(l), simplify(r)) match {
      case (_, CoreExpr.True) => Some((Seq(), Seq()))
      case (l, r) if l == r => Some((Seq(), Seq()))
      case _ => None
    }
    private[this] def simplify(e: Expr): Expr = e match {
      case CoreExpr.And(es) =>
        es.map(simplify).filterNot(_ == CoreExpr.True) match {
          case Seq() => CoreExpr.True
          case Seq(e1) => e1
          case Seq(es @ _*) =>
            if (es.contains(CoreExpr.False)) CoreExpr.False
            else CoreExpr.And(es.asInstanceOf[Seq[CoreExpr]]) // TODO: Why typecheck failed
        }
      case e => e
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

    private[this] def pos(v: Value) = valueRepo.getPos(v) match {
      case Some(p) =>
        s"${query.lineNum(p)}:${query.columnNum(p)}"
      case None =>
        s"???"
    }
    private[this] def runSMT(constraints: Seq[LogicConstraint], binding: Map[Value.Naked, Pred]): Seq[Conflict] = {
      val smt = SMT.newContext()
      val compiler = new LogicCompiler(smt.ctx)

      dprint("SMT Logic:")
      constraints.foreach { c =>
        def valueString(v: Value) = s"$v: ${binding(v.naked)}"
        def show(v: Value): String =
          s"${pos(v)} ${valueString(v)}"

        dprint(s"${pos(c.constraint.focus)} ${c.constraint.focus}}")
        c.constraint.env.conds.foreach { v =>
          dprint("  COND:", show(v))
        }
        c.constraint.env.unconds.foreach { v =>
          dprint("  UNCOND:", show(v))
        }
        dprint("  Constraint:", c.constraint)
        dprint("  Logic:", c.logic)
        dprint("  Compiled:", compiler.compileBoolean(c.logic))
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
