package com.todesking.prety.universe

import com.todesking.prety.{ Lang, Logic }

trait Preds { self: ForeignTypes with ForeignTypeOps with Queries with Values with Props with Envs with Exprs with Conflicts =>
  abstract class Pred {
    def tpe: TypeSym

    // where this.tpe <:< key.targetType
    // where pred.tpe <:< key.tpe
    def prop(key: PropKey): PropPred

    // where pred.tpe <:< key.tpe
    def definedProps: Map[PropKey, PropPred]

    // where this.tpe <:< tpe
    // where _.tpe <:< tpe
    // def as(tpe: TypeSym): Pred

    def substitute(mapping: Map[Value, Value]): Pred

    def &(rhs: Pred): Pred

    def upcast(newType: TypeSym): Pred = {
      require(tpe <:< newType)
      Pred(newType, definedProps.filterKeys { k => newType <:< k.targetType })
    }
  }

  object Pred {
    def and(ps: Seq[Pred]): Pred =
      ps.reduceOption(_ & _) getOrElse True

    def apply(targetType: TypeSym, ppreds: Map[PropKey, PropPred]): Pred = {
      require(ppreds.keys.forall(_.targetType <:< targetType))
      new Pred {
        override def tpe = targetType
        override def prop(key: PropKey) = ppreds.get(key) getOrElse PropPred.True
        override def definedProps = ppreds
        override def substitute(mapping: Map[Value, Value]) = apply(targetType, ppreds.mapValues(_.substitute(mapping)))
        override def &(rhs: Pred) = ???
        override def toString =
          definedProps.map { case (k, v) => s"$k: $v" }.mkString("{", ", ", "}")
      }
    }
    case object True extends Pred {
      override def tpe = query.types.nothing
      override def prop(key: PropKey) = throw new IllegalArgumentException("True pred has no props")
      override def definedProps = Map()
      override def substitute(mapping: Map[Value, Value]) = this
      override def &(rhs: Pred) = rhs
      override def toString = "{}"
    }

    def compile(props: Map[String, Lang.Expr], targetType: TypeSym, env: Env): Pred =
      Pred(
        targetType,
        props.map {
          case (name, expr) =>
            val key = env.findProp(name, targetType)
            key -> env.findWorld(key.tpe).buildPred(
              Compiler.compile(expr, env))
        })

    def exactInt(value: Value, v: Int): Pred =
      compile(Map("_" -> Lang.Expr.Op(Lang.Expr.TheValue, "==", Lang.Expr.LitInt(v))), query.types.int, buildEnv(Map(), value))
  }

  trait PropPred {
    def substitute(mapping: Map[Value, Value]): PropPred
  }
  object PropPred {
    val True = CorePred(CoreExpr.BOOL_Lit(true))
  }

  trait World {
    val tpe: TypeSym
    def buildPred(expr: Expr): PropPred
    def solveConstraint(lhs: PropPred, rhs: PropPred): (Seq[Logic], Seq[Conflict])
  }

  object Compiler {
    import Lang.{ Expr => E }
    val CE = CoreExpr
    def compile(ast: Lang.Expr, env: Env): Expr = ast match {
      case E.TheValue =>
        CE.TheValue(env.theValue.tpe)
      case E.Ident(name) =>
        CE.ValueRef(env.findValue(name))
      case E.Select(expr, name) =>
        ???
      case E.LitInt(value) =>
        CE.INT_Lit(value)
      case E.Op(lhs, op, rhs) =>
        val l = compile(lhs, env)
        val r = compile(rhs, env)
        env.findOp(l.tpe, op).apply(l, r)
    }
  }

  class IntWorld extends World {
    override val tpe = query.types.int

    override def buildPred(expr: Expr): CorePred = expr match {
      case e: CoreExpr => CorePred(e)
    }

    override def solveConstraint(lhs: PropPred, rhs: PropPred) = (lhs, rhs) match {
      case (CorePred(l), CorePred(r)) =>
        (Seq(toLogic(l) --> toLogic(r)), Seq())
    }

    private[this] val E = CoreExpr
    private[this] def toLogic(e: CoreExpr): Logic = ???
  }

  case class CorePred(expr: CoreExpr) extends PropPred {
    override def substitute(mapping: Map[Value, Value]): CorePred =
      CorePred(expr.substitute(mapping))
  }

}
