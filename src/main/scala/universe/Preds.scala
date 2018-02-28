package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Preds { self: ForeignTypes with Queries with Values with Props =>
  abstract class Pred {
    def tpe: TypeSym

    // where this.tpe <:< key.targetType
    // where pred.tpe <:< key.tpe
    def prop(key: PropKey): PropPred

    // where pred.tpe <:< key.tpe
    def definedProps: Map[PropKey, PropPred]

    // where this.tpe <:< tpe
    // where _.tpe == tpe
    // def as(tpe: TypeSym): Pred

    def substitute(mapping: Map[Value, Value]): Pred

    def &(rhs: Pred): Pred
  }

  class Env(
    val props: Map[String, PropKey],
    val values: Map[String, Value],
    val theValue: Value,
    val ops: Map[(TypeSym, String), (PropExpr, PropExpr) => PropExpr]) {
    private[this] def nf(kind: String, key: String) = throw new RuntimeException(s"$kind $key not found")
    def findProp(name: String, targetType: TypeSym): PropKey = name match {
      case "_" =>
        PropKey(targetType.toString, targetType, targetType)
      case name =>
        props.get(name) getOrElse nf("Property", name)
    }
    def findValue(name: String): Value =
      values.get(name) getOrElse nf("Value", name)
    def findOp(tpe: TypeSym, name: String): (PropExpr, PropExpr) => PropExpr =
      ops.get(tpe -> name) getOrElse nf("Operator", s"$tpe.$name")
  }

  object Pred {
    def and(ps: Seq[Pred]): Pred =
      ps.reduceOption(_ & _) getOrElse True

    def apply(targetType: TypeSym, ppreds: Map[PropKey, PropPred]): Pred = ???

    case object True extends Pred {
      override def tpe = query.types.nothing
      override def prop(key: PropKey) = throw new IllegalArgumentException("True pred has no props")
      override def definedProps = Map()
      override def substitute(mapping: Map[Value, Value]) = this
      override def &(rhs: Pred) = rhs
    }

    def compile(props: Map[String, Lang.Expr], targetType: TypeSym, env: Env): Pred =
      Pred(
        targetType,
        props.map {
          case (name, expr) =>
            val key = env.findProp(name, targetType)
            key -> compile1(key, expr, env)
        })

    private[this] def compile1(key: PropKey, expr: Lang.Expr, env: Env): PropPred =
      compileExpr(expr, env).toPropPred

    private[this] def compileExpr(expr: Lang.Expr, env: Env): PropExpr = {
      import Lang.{ Expr => L }
      val E = PropExpr
      expr match {
        case L.TheValue =>
          E.ValueRef(env.theValue)
        case L.Ident(name) =>
          E.ValueRef(env.findValue(name))
        case L.Select(expr, name) =>
          ???
        case L.LitInt(v) =>
          E.LitInt(v)
        case L.Op(lhs, name, rhs) =>
          val el = compileExpr(lhs, env)
          val er = compileExpr(rhs, env)
          env.findOp(el.tpe, name).apply(el, er)
      }
    }

    def exactInt(v: Int): Pred =
      compile(Map("_" -> Lang.Expr.Op(Lang.Expr.TheValue, "==", Lang.Expr.LitInt(v))), query.types.int, ???)
  }
  sealed abstract class PredHolder {
    def pred(binding: Map[Value, Pred]): Pred
    def toValue: Option[Value]
  }
  object PredHolder {
    case class Variable(value: Value) extends PredHolder {
      def substitute(mapping: Map[Value, Value]) =
        Substitute(mapping, this)
      override def pred(binding: Map[Value, Pred]) = binding(value)
      override def toValue = Some(value)
      override def toString = s"?($value)"
    }

    case class Substitute(mapping: Map[Value, Value], original: PredHolder) extends PredHolder {
      override def pred(binding: Map[Value, Pred]) =
        original.pred(binding).substitute(mapping)
      override def toValue = original.toValue
      override def toString = s"[${mapping.toSeq.map { case (k, v) => s"$k -> $v" }.mkString(", ")}](${original.toValue.get})"
    }
  }
}
