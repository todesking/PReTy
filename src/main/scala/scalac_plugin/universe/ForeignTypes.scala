package com.todesking.prety.scalac_plugin.universe

trait ForeignTypes {
  type Pos
  type Tree

  // vals and defs
  type DefSym

  type TypeSym

  val query: QueryAPI
  trait QueryAPI {
    def name(f: DefSym): String
    def paramss(f: DefSym): Seq[Seq[DefSym]]
    def refinementSrc(f: DefSym): Seq[String]
  }
}
