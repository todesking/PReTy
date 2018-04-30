package com.todesking.prety.universe

import com.todesking.prety.{ Lang }

trait Preds { self: ForeignTypes with Values with Props with Envs with Exprs with Conflicts with Worlds with Debugging =>
  case class Pred(tpe: TypeSym, self: Expr, definedProps: Map[PropKey, Pred]) {
    // where this.tpe <:< key.targetType
    // where pred.tpe <:< key.tpe
    // TODO: check requirements of key
    def prop(key: PropKey): Pred = definedProps.get(key) getOrElse Pred.True

    // where this.tpe <:< tpe
    // where _.tpe <:< tpe
    // def as(tpe: TypeSym): Pred
    def substitute(mapping: Map[Value, Value]): Pred =
      Pred(tpe, self.substitute(mapping), definedProps.mapValues(_.substitute(mapping)))

    def &(rhs: Pred): Pred = {
      require(tpe == rhs.tpe || this == Pred.True || rhs == Pred.True) // TODO: make upper bound type
      if (this == Pred.True) rhs
      else if (rhs == Pred.True) this
      else {
        Pred(
          tpe,
          self & rhs.self,
          (definedProps.toSeq ++ rhs.definedProps.toSeq)
            .groupBy(_._1)
            .map { case (k, kvs) => k -> kvs.map(_._2).reduce(_ & _) }
            .toMap)
      }
    }

    def cast(newType: TypeSym): Pred = {
      // TODO: really accept newType <:< tpe ?????
      require(tpe <:< newType || newType <:< tpe, s"$tpe !<:> $newType")
      val newSelf = if (newType == tpe) self else CoreExpr.True
      Pred(newType, newSelf, definedProps.filterKeys(_.isTarget(newType)))
    }

    def messageString: String =
      s"($self)${
        definedProps.map { case (k, v) => s"${k.name}: ${v.src}" }.mkString("{", ", ", "}")
      }"
    def src: String = messageString

    override def toString =
      s"($self)${
        definedProps.map { case (k, v) => s"${k.name}: $v" }.mkString("{", ", ", "}")
      }"
  }

  object Pred {
    def and(ps: Seq[Pred]): Pred =
      ps.reduceOption(_ & _) getOrElse True

    val True = Pred(query.types.nothing, CoreExpr.True, Map())

    def compile(w: World, props: Map[String, Lang.Expr], targetType: TypeSym, env: Env): Pred = {
      val self = props.get("_").map { s =>
        Expr.compile(w, s, env, targetType)
      } getOrElse CoreExpr.True
      Pred(
        targetType,
        self,
        props.filterNot(_._1 == "_").map {
          case (name, expr) =>
            val key = w.findPropKey(name, targetType)
            val pred = Pred(
              key.tpe,
              Expr.compile(w, expr, env, key.tpe),
              Map())
            key -> pred
        })
    }

    def exactInt(v: Int): Pred =
      Pred(
        query.types.int,
        CoreExpr.INT_EQ(CoreExpr.TheValue(query.types.int), CoreExpr.INT_Lit(v)),
        Map())
    def exactBoolean(v: Boolean): Pred =
      Pred(
        query.types.boolean,
        CoreExpr.BOOL_EQ(CoreExpr.TheValue(query.types.boolean), CoreExpr.BOOL_Lit(v)),
        Map())
  }

  sealed abstract class UnknownPred {
    def reveal(binding: Map[Value.Naked, Pred]): Pred = revealOpt(binding) getOrElse {
      throw new RuntimeException(s"Can't reveal $this(env=$binding)")
    }
    def revealOpt(binding: Map[Value.Naked, Pred]): Option[Pred]
    def toValue: Option[Value]
  }
  object UnknownPred {
    def ref(v: Value, k: PropKey): UnknownPred =
      Ref(OfValue(v), k)

    case class Ref(self: UnknownPred, key: PropKey) extends UnknownPred {
      override def revealOpt(binding: Map[Value.Naked, Pred]) =
        self.revealOpt(binding)
          .map { pred =>
            pred.prop(key)
          }
      override def toValue = self.toValue
      override def toString = s"$self.$key"
    }

    case class OfValue(value: Value) extends UnknownPred {
      def substitute(mapping: Map[Value, Value]) =
        Substitute(mapping, this)
      override def revealOpt(binding: Map[Value.Naked, Pred]) = binding.get(value.naked)
      override def toValue = Some(value)
      override def toString = s"?($value)"
    }

    case class Substitute(mapping: Map[Value, Value], original: UnknownPred) extends UnknownPred {
      override def revealOpt(binding: Map[Value.Naked, Pred]) =
        original.revealOpt(binding).map(_.substitute(mapping))
      override def toValue = original.toValue
      override def toString = s"[${mapping.toSeq.map { case (k, v) => s"$k -> $v" }.mkString(", ")}](${original.toValue.get})"
    }
  }
}
