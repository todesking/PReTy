package com.todesking.prety.universe

trait Queries { self: ForeignTypes =>
  val query: QueryAPI
  trait QueryAPI {
    def name(f: DefSym): String
    def paramss(f: DefSym): Seq[Seq[DefSym]]
    def returnType(f: DefSym): TypeSym
    def thisType(f: DefSym): TypeSym
    def refinementSrc(f: DefSym): Seq[String]
    def pos(f: DefSym): Pos

    def isAccessor(f: DefSym): Boolean
    def unwrapAccessor(f: DefSym): DefSym

    def isLocal(f: DefSym): Boolean

    val isDebugMode: Boolean

    val emptyPos: Pos
    def lineNum(p: Pos): Int
    def columnNum(p: Pos): Int
    def samePos(l: Pos, r: Pos): Boolean

    def <:<(lhs: TypeSym, rhs: TypeSym): Boolean

    def lookupMember(self: TypeSym, name: String, ret: TypeSym, paramss: Seq[Seq[TypeSym]]): DefSym

    val types: TypesAPI
    trait TypesAPI {
      val nothing: TypeSym
      val any: TypeSym
      val int: TypeSym
      val boolean: TypeSym
      def fromName(fqn: String): TypeSym
    }
  }
}
