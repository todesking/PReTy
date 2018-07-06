package com.todesking.prety.universe

trait ForeignTypes {
  type Pos >: Null <: AnyRef
  type Tree >: Null <: AnyRef

  // vals and defs
  type DefSym >: Null <: AnyRef

  type TypeSym >: Null <: AnyRef

  implicit class TypeSymOps(self: TypeSym) {
    def <:<(rhs: TypeSym): Boolean = query.<:<(self, rhs)
  }

  val query: QueryAPI
  trait QueryAPI {
    def name(f: DefSym): String
    def paramss(f: DefSym): Seq[Seq[DefSym]]
    def returnType(f: DefSym): TypeSym
    def thisType(f: DefSym): TypeSym
    def refineAnnotations(f: DefSym): Seq[String]
    def refineSimpleAnnotations(f: DefSym): Seq[String]
    def pos(f: DefSym): Pos

    def stableValueMembers(t: TypeSym): Seq[DefSym]

    def isAccessor(f: DefSym): Boolean
    def unwrapAccessor(f: DefSym): DefSym
    def isPrimaryCtor(f: DefSym): Boolean
    def isStable(f: DefSym): Boolean
    def isConstructor(f: DefSym): Boolean

    def isLocal(f: DefSym): Boolean

    val isDebugMode: Boolean

    val emptyPos: Pos
    def lineNum(p: Pos): Int
    def columnNum(p: Pos): Int
    def samePos(l: Pos, r: Pos): Boolean

    def <:<(lhs: TypeSym, rhs: TypeSym): Boolean
    def baseTypes(t: TypeSym): Seq[TypeSym]

    def lookupMembers(self: TypeSym, name: String, ret: TypeSym, paramss: Seq[Seq[TypeSym]]): Seq[DefSym]
    // TODO: Option[DefSym]
    def lookupMember(self: TypeSym, name: String, ret: TypeSym, paramss: Seq[Seq[TypeSym]]): DefSym = {
      val ms = lookupMembers(self, name, ret, paramss)
      val pat = s"$self.$name ${paramss.map(_.mkString("(", ", ", ")")).mkString("")}: $ret, ${ms.mkString(", ")}"
      // TODO: I need proper method overloading resolution :(
      // if (ms.size > 1)
      //   throw new RuntimeException(s"Multiple member candidate for $pat")
      ms.headOption getOrElse { throw new RuntimeException(s"Member not found for $pat") }
    }

    val types: TypesAPI
    trait TypesAPI {
      val nothing: TypeSym
      val any: TypeSym
      val anyRef: TypeSym
      val int: TypeSym
      val boolean: TypeSym
      def fromName(fqn: String): TypeSym
    }
  }
}
