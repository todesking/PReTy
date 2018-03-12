package com.todesking.prety.universe

trait Conflicts { self: Values with Constraints =>
  case class Conflict(constraint: GroundConstraint) {
    def message: String = constraint.messageString
    def focus: Value = constraint.focus
  }
}
