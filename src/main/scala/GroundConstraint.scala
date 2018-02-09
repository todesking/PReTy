package com.todesking.prety

case class GroundConstraint(lhs: Pred, rhs: Pred) {
  override def toString = s"$lhs <= $rhs"
}

