package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Macros { self: ForeignTypes with Exprs with Envs with Worlds =>
  abstract class Macro {
    def name: String
    def select(name: String): Either[Macro, Expr] = throw new RuntimeException
    def apply(self: Option[Expr], args: Seq[Expr], env: Env): Either[Macro, Expr] = throw new RuntimeException
    def expr: Expr = throw new RuntimeException
  }
  object Macro {
    def fun(fullName: String, params: TypeSym*)(f: PartialFunction[(Seq[Expr], Env), Expr]): Macro =
      new Macro {
        override def name = fullName
        override def apply(self: Option[Expr], args: Seq[Expr], env: Env) = {
          require(params.size == args.size, s"$fullName: required parameter size is ${params.size} but ${args.size}")
          require(params.zip(args).forall { case (p, a) => a.tpe <:< p }, s"$fullName: required params is (${params.mkString(", ")}) but (${args.mkString(", ")})")
          f.lift.apply((self.toSeq ++ args, env)).map(Right.apply) getOrElse { throw new RuntimeException(s"Some requirement failed on $fullName: args=(${args.mkString(", ")})") }
        }
      }
    def dict(fullName: String)(members: (String, Macro)*): Macro = {
      val memberMap = members.toMap
      new Macro {
        override def name = fullName
        override def select(name: String) = Left(memberMap(name))
      }
    }
    def method(w: World, name: String, src: String, ret: TypeSym, paramss: Seq[Seq[(String, TypeSym)]]): Macro = {
      require(paramss.size == 1, "TODO")
      val body = Lang.parseExpr(src)
      fun(name, paramss(0).map(_._2): _*) {
        case (Seq(self, args @ _*), env) =>
          val sub = Map("this" -> self) ++ paramss.flatten.map(_._1).zip(args).toMap
          Expr.compile(w, body, env, ret, sub)
      }
    }
  }
}
