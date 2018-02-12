package com.todesking.prety.universe

trait Queries { self: ForeignTypes =>
  val query: QueryAPI
  trait QueryAPI {
    def name(f: DefSym): String
    def paramss(f: DefSym): Seq[Seq[DefSym]]
    def refinementSrc(f: DefSym): Seq[String]
    def emptyPos: Pos
  }
}
