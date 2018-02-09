package com.todesking.prety

sealed abstract class Pred {
  def substitute(mapping: Map[Value, Value]): Pred
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

