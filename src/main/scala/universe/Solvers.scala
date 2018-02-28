package com.todesking.prety.universe

trait Solvers { self: Graphs with Constraints with Conflicts with Preds =>
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
      Seq()
    }
  }
}
