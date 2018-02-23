package com.todesking.prety.universe

trait Props { self: ForeignTypes =>
  case class PropType(sym: TypeSym)

  case class PropKey(name: String, tpe: PropType)

  trait PropPred {
    def tpe: PropType
  }
}
