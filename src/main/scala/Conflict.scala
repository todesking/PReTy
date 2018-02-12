package com.todesking.prety

case class Conflict(constraint: GroundConstraint) {
  def message: String = constraint.toString
  def focus: Value = constraint.focus
}
