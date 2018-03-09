package com.todesking.prety.universe

import com.todesking.prety.{ Lang }

trait Preds { self: ForeignTypes with ForeignTypeOps with Queries with Values with Props with Envs with Exprs with Conflicts with Worlds =>
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
              Expr.compile(expr, env))
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

  case class CorePred(expr: CoreExpr) extends PropPred {
    override def substitute(mapping: Map[Value, Value]): CorePred =
      CorePred(expr.substitute(mapping))
    override def toString = expr.toString
  }

}
