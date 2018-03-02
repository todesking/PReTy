package com.todesking.prety.universe

import com.todesking.prety.Logic

trait Solvers { self: ForeignTypes with Graphs with Constraints with Conflicts with Preds with Envs with Props =>
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

    private[this] def runSMT(l: Seq[Logic]): Seq[Conflict] = ???
  }
}
