package com.todesking.prety.universe

trait Macros { self: ForeignTypes with Exprs =>
  abstract class Macro {
    def name: String
    def select(name: String): Either[Macro, Expr] = throw new RuntimeException
    def apply(args: Seq[Expr]): Either[Macro, Expr] = throw new RuntimeException
    def expr: Expr = throw new RuntimeException
  }
  object Macro {
    def fun(fullName: String, params: TypeSym*)(f: PartialFunction[Seq[Expr], Expr]): Macro =
      new Macro {
        override def name = fullName
        override def apply(args: Seq[Expr]) = {
          require(params.size == args.size, s"$fullName: required parameter size is ${params.size} but ${args.size}")
          require(params.zip(args).forall { case (p, a) => a.tpe <:< p }, s"$fullName: required params is (${params.mkString(", ")}) but (${args.mkString(", ")})")
          f.lift.apply(args).map(Right.apply) getOrElse { throw new RuntimeException(s"Some requirement failed on $fullName: args=(${args.mkString(", ")})") }
        }
      }
    def dict(fullName: String)(members: (String, Macro)*): Macro = {
      val memberMap = members.toMap
      new Macro {
        override def name = fullName
        override def select(name: String) = Left(memberMap(name))
      }
    }
  }
}
