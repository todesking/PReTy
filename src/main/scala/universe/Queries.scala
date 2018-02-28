package com.todesking.prety.universe

trait Queries { self: ForeignTypes =>
  val query: QueryAPI
  trait QueryAPI {
    def name(f: DefSym): String
    def paramss(f: DefSym): Seq[Seq[DefSym]]
    def returnType(f: DefSym): TypeSym
    def thisType(f: DefSym): TypeSym
    def refinementSrc(f: DefSym): Seq[String]
    def emptyPos: Pos
    def <:<(lhs: TypeSym, rhs: TypeSym): Boolean

    val types: TypesAPI
    trait TypesAPI {
      def nothing: TypeSym
      def int: TypeSym
      def boolean: TypeSym
    }
  }
}
