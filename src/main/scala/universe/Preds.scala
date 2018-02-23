package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Preds { self: Values with Props =>
sealed abstract class Pred {
  def substitute(mapping: Map[Value, Value]): Pred
  def propPreds: Map[PropKey, PropPred] = ???
}
object Pred {
  def and(ps: Seq[Pred]): Pred =
    if (ps.isEmpty) True
    else if (ps.size == 1) ps.head
    else And(ps)
  case object True extends Pred {
    override def substitute(mapping: Map[Value, Value]) = this
  }
  case object False extends Pred {
    override def substitute(mapping: Map[Value, Value]) = this
  }
  case class And(preds: Seq[Pred]) extends Pred {
    override def substitute(mapping: Map[Value, Value]) =
      And(preds.map(_.substitute(mapping)))
    override def toString = preds.mkString(" && ")
  }

  case class Expr(expr: Lang.AST, env: Map[String, Value]) extends Pred {
    override def substitute(mapping: Map[Value, Value]) =
      Expr(
        expr,
        env.mapValues { v =>
          mapping.get(v) getOrElse v
        })
    override def toString = {
      val names = expr.names
      val activeEnv = env.filter { case (k, v) => names(k) }

      if (activeEnv.isEmpty) s"{$expr}"
      else s"{$expr}[${activeEnv.map { case (k, v) => s"$k -> $v" }.mkString(", ")}]"
    }
  }
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
