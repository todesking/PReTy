package com.todesking.prety.universe

trait Props { self: ForeignTypes with Queries with ForeignTypeOps with Values =>
  case class PropKey(name: String, targetType: TypeSym, tpe: TypeSym) {
    override def toString = s"[$name($targetType): $tpe]"
  }
}
