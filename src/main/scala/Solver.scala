package com.todesking.prety

object Solver {
  def solve(g: Graph): Seq[Conflict] = {
    val (nonTrivial, trivialConflicts) = solveTrivial(g.groundConstraints)
    trivialConflicts ++ nonTrivial.map { c =>
      Conflict(c)
    }
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
}
