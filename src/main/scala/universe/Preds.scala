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

    def cast(newType: TypeSym): Pred = {
      require(tpe <:< newType || newType <:< tpe, s"$tpe !<:> $newType")
      Pred(newType, definedProps.filterKeys { k => newType <:< k.targetType })
    }

    def messageString: String
  }

  object Pred {
    def and(ps: Seq[Pred]): Pred =
      ps.reduceOption(_ & _) getOrElse True

    case class Custom(tpe: TypeSym, ppreds: Map[PropKey, PropPred]) extends Pred {
      require(ppreds.keys.forall(_.tpe <:< tpe))
      override def prop(key: PropKey) = ppreds.get(key) getOrElse PropPred.True
      override def definedProps = ppreds
      override def substitute(mapping: Map[Value, Value]) = apply(tpe, ppreds.mapValues(_.substitute(mapping)))
      override def &(rhs: Pred) = rhs match {
        case True => this
        case pred =>
          require(pred.tpe <:< this.tpe)
          val props = definedProps.keySet ++ pred.cast(tpe).definedProps.keySet
          Pred(
            tpe,
            props.toSeq.map { key =>
              key -> (prop(key) & pred.prop(key))
            }.toMap)
      }
      override def toString =
        definedProps.map { case (k, v) => s"${k.name}: $v" }.mkString("{", ", ", "}")
      override def messageString =
        definedProps.map { case (k, v) => s"${k.name}: ${v.src}" }.mkString("{", ", ", "}")
    }

    def apply(targetType: TypeSym, ppreds: Map[PropKey, PropPred]): Pred =
      Custom(targetType, ppreds)

    case object True extends Pred {
      override def tpe = query.types.any
      override def prop(key: PropKey) = throw new IllegalArgumentException("True pred has no props")
      override def definedProps = Map()
      override def substitute(mapping: Map[Value, Value]) = this
      override def &(rhs: Pred) = rhs
      override def toString = "{}"
      override def messageString = "{}"
    }

    def compile(props: Map[String, Lang.Expr], targetType: TypeSym, env: Env): Pred =
      Pred(
        targetType,
        props.map {
          case (name, expr) =>
            val key = env.findPropKey(name, targetType)
            val pred = env.findProp(key.tpe).buildPred(
              expr.toString,
              Expr.compile(expr, env, key.tpe))
            key -> pred
        })

    def exactInt(value: Value, v: Int): Pred =
      compile(
        Map("_" -> Lang.Expr.Op(Lang.Expr.TheValue, "==", Lang.Expr.LitInt(v))),
        query.types.int,
        buildEnv(Map()))
  }

  trait PropPred {
    def substitute(mapping: Map[Value, Value]): PropPred
    def src: String
    def &(rhs: PropPred): PropPred
  }
  object PropPred {
    val True = CorePred("true", CoreExpr.BOOL_Lit(true))
  }

  case class CorePred(src: String, expr: CoreExpr) extends PropPred {
    override def &(rhs: PropPred): PropPred = rhs match {
      case PropPred.True => this
      case CorePred(s, e) =>
        CorePred(s"($src) & ($e)", expr & e)
      case _ => ???
    }
    override def substitute(mapping: Map[Value, Value]): CorePred =
      CorePred(src, expr.substitute(mapping))
    override def toString = expr.toString
  }

}
