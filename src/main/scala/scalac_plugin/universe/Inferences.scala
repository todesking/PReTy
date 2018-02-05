package com.todesking.prety.scalac_plugin.universe

import scala.language.implicitConversions

trait Inferences { self: ASTs with Preds with Constraints =>
  implicit def valueToPredHolder(v: Value): PredHolder.Variable =
    PredHolder.Variable(v)

  implicit class ValueOps(self: Value) {
    def *<:=(rhs: PredHolder): Constraint = Constraint.FocusLeft(self, rhs)
  }

}
