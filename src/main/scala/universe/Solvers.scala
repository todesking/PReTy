package com.todesking.prety.universe

import com.todesking.prety.Logic
import com.todesking.prety.SMT
import com.todesking.prety.LogicCompiler

trait Solvers { self: ForeignTypes with Values with Graphs with Constraints with Conflicts with Preds with Envs with Preds with Props with Exprs with Worlds with Debugging =>
  class Binding(b: Map[Value.Naked, Pred]) {
    def roots: Set[Value.Root] = ???
    def apply(v: Value.Naked): Pred = ???
    def expr(v: Value.Naked): Expr = apply(v).self
  }

  class Solver(world: World) {
    private[this] val valueRepo = world.values

    def solve(g: Graph): Seq[Conflict] =
      solveSMT(g.groundConstraints, g.binding)

    def solveSMT(constraints: Seq[GroundConstraint], binding: Map[Value.Naked, Pred]): Seq[Conflict] = {
      val b = new Binding(binding)
      val (ls, cs) =
        constraints.map(compileConstraint(_, b))
          .foldLeft((Seq.empty[LogicConstraint], Seq.empty[Conflict])) {
            case ((al, ac), (l, c)) =>
              (al :+ l, ac ++ c)
          }
      cs ++ runSMT(ls, binding) // NOTE: binding is just for debugging purpose
    }

    private[this] def compileConstraint(c: GroundConstraint, binding: Binding): (LogicConstraint, Seq[Conflict]) = {

      def allValues(p1: Pred, p2: Pred): Set[Value.Naked] = {
        val v1: Set[Value.Naked] = p1.customSelf.toSet.flatMap{a:Expr => a.values} // TODO: Why inference failed?
        val v2 : Set[Value.Naked]= p2.customSelf.toSet.flatMap{a: Expr => a.values}
        val v3 : Set[Value.Naked]= (p1.propKeys ++ p2.propKeys).flatMap { k =>
          // TODO: if(p1.hasKey(k) && !p2.hasKey(k))
          if(p1.customized(k) || p2.customized(k)) allValues(p1.prop(k), p2.prop(k)): Set[Value.Naked]
          else Set.empty[Value.Naked]
        }
        v1 ++ v2 ++ v3
      }

      def expand(seed: Set[Value.Naked], done: Set[Value.Naked]): Set[Value.Naked] = ???

      val depValues = expand(
        allValues(c.lhs, c.rhs) ++ (c.env.conds ++ c.env.unconds).flatMap(v => binding.expr(v.naked).values),
        Set()
      )

      val env: Map[Value.Naked, Expr] = depValues.map { v => v -> binding.expr(v) }.toMap
      def compileEnv(v: Value.Naked, e: Expr): Logic.LBool  = {
        compileTrivial(v, e) getOrElse {
          world.prop(v.tpe).toLogic(e, v)
        }
      }

      val envLogic: Logic.LBool = Logic.and(env.map { case (v, e) =>
        compileEnv(v, e)
      })

      def flatten(p1: Pred, p2: Pred): Map[Value.Naked, (Expr, Expr)] = ???
      val flatConstraints: Map[Value.Naked, (Expr, Expr)] = flatten(c.lhs, c.rhs)

      val logics: Seq[Logic.LBool] = flatConstraints.map { case (v, (l, r)) =>
        (simplify(l), simplify(r)) match {
          case (_, Expr.True) => Logic.True
          case (l, r) if l == r => Logic.True
          case (l, r) =>
            // DF{id: Int} <: DF{id: Int, name: String}
      }
      val conflicts : Seq[Conflict] = ???
      (LogicConstraint(c, Logic.and(logics).universalQuantifiedForm), conflicts)
    }

    // TODO: compileCoreExpr
    private[this] def compileTrivial(v: Value.Naked, e: Expr): Option[Logic.LBool] = e match {
      case CoreExpr.BOOL_Lit(b) => Some(Logic.BValue(b))
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
