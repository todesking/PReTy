package com.todesking.prety.universe

trait ForeignTypes {
  type Pos >: Null <: AnyRef
  type Tree >: Null <: AnyRef

  // vals and defs
  type DefSym >: Null <: AnyRef

  type TypeSym >: Null <: AnyRef
}
