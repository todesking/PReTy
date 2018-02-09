package com.todesking.prety.scalac_plugin.universe

trait ForeignTypes {
  type Pos >: Null <: AnyRef
  type Tree >: Null <: AnyRef

  // vals and defs
  type DefSym >: Null <: AnyRef

  type TypeSym >: Null <: AnyRef

  val query: QueryAPI
  trait QueryAPI {
    def name(f: DefSym): String
    def paramss(f: DefSym): Seq[Seq[DefSym]]
    def refinementSrc(f: DefSym): Seq[String]
  }

  def emptyPos: Pos
}
