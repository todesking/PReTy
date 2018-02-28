package com.todesking.prety.universe

trait ForeignTypeOps { self: ForeignTypes with Queries =>
  implicit class TypeSymOps(self: TypeSym) {
    def <:<(rhs: TypeSym): Boolean = query.<:<(self, rhs)
  }

}
