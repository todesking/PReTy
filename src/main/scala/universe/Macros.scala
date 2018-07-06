package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Macros { self: ForeignTypes with Exprs with Envs with Worlds =>
  abstract class Macro {
    def name: String
    def select(env: MacroEnv, name: String): Either[Macro, Expr] = throw new RuntimeException
    def apply(env: MacroEnv, self: Option[Expr], args: Seq[Expr]): Either[Macro, Expr] = throw new RuntimeException
    def expr(env: MacroEnv): Expr = throw new RuntimeException
  }
  object Macro {
    def fun(fullName: String, params: TypeSym*)(f: PartialFunction[(MacroEnv, Seq[Expr]), Expr]): Macro =
      new Macro {
        override def name = fullName
        override def apply(env: MacroEnv, self: Option[Expr], args: Seq[Expr]) = {
          require(params.size == args.size, s"$fullName: required parameter size is ${params.size} but ${args.size}")
          require(params.zip(args).forall { case (p, a) => a.tpe <:< p }, s"$fullName: required params is (${params.mkString(", ")}) but (${args.mkString(", ")})")
          f.lift.apply((env, self.toSeq ++ args)).map(Right.apply) getOrElse { throw new RuntimeException(s"Some requirement failed on $fullName: args=(${args.mkString(", ")})") }
        }
      }
    def dict(fullName: String)(members: (String, Macro)*): Macro = {
      val memberMap = members.toMap
      new Macro {
        override def name = fullName
        override def select(env: MacroEnv, name: String) = Left(memberMap(name))
      }
    }
    def method(name: String, body: Lang.Expr, ret: TypeSym, paramss: Seq[Seq[(String, TypeSym)]]): Macro = {
      require(paramss.size == 1, "TODO")
      fun(name, paramss(0).map(_._2): _*) {
        case (env, Seq(self, args @ _*)) =>
          val sub = Map("this" -> self) ++ paramss.flatten.map(_._1).zip(args).toMap
          Expr.compile(env, sub, body, ret)
      }
    }
  }

  class MacroEnv(
    globalMakros: Map[String, Macro],
    customMembers: Map[DefSym, Macro]) {
    def global(name: String): Option[Macro] = globalMakros.get(name)
    def member(tpe: TypeSym, name: String, argss: Seq[Seq[TypeSym]]): Option[Macro] =
      Some(query.lookupMember(tpe, name, query.types.any, argss)).flatMap { f =>
        customMembers.get(f) orElse {
          ???
        }
      }
  }

}
