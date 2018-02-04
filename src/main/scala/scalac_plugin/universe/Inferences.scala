package com.todesking.prety.scalac_plugin.universe

import scala.language.implicitConversions

trait Inferences { self: ASTs with Preds with Constraints =>
  implicit def valueToPredHolder(v: Value): PredHolder.Variable =
    PredHolder.Variable(v)

  implicit def predToPredHolder(p: Pred): PredHolder.Ground =
    PredHolder.Ground(p)

  implicit class ValueOps(self: Value) {
    def *<:=(rhs: PredHolder): Constraint = Constraint.FocusLeft(self, rhs)
    def *=:=(rhs: Pred): Constraint = Constraint.Bind(self, rhs)
  }

}
