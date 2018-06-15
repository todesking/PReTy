package com.todesking.prety.universe

import com.todesking.prety.Lang

trait Preds { self: ForeignTypes with Values with Props with Envs with Exprs with Conflicts with Worlds with Templates with Debugging =>
  // TODO: Expr -> Expr.Boolean
  abstract class Pred {
    def tpe: TypeSym

    def self: Expr

    // where this.tpe <:< key.targetType
    // where pred.tpe <:< key.tpe
    // TODO: check requirements of key
    def prop(key: PropKey): Pred

    def propKeys: Set[PropKey]
    def customPropKeys: Set[PropKey]
    def customized(k: PropKey): Boolean =
      customPropKeys.contains(k)

    // where this.tpe <:< tpe
    // where _.tpe <:< tpe
    // def as(tpe: TypeSym): Pred
    def substitute(mapping: Map[Value, Value]): Pred =
      Pred.Substitute(this, mapping)

    def &(rhs: Pred): Pred = {
      require(tpe == rhs.tpe) // TODO: make upper bound type
      Pred.Custom(
        this,
        Some(self & rhs.self),
        propKeys.toSeq // TODO: propKeys ++ rhs.propKeys ?
          .map { k => k -> (prop(k) & rhs.prop(k)) }
          .toMap)
    }

    def cast(newType: TypeSym): Pred = {
      // TODO: really accept newType <:< tpe ?????
      require(tpe <:< newType || newType <:< tpe, s"$tpe !<:> $newType")
      // TODO: should check prop references in expr?
      Pred.Cast(this, newType)
    }

    def messageString: String =
      s"($self)${
        customPropKeys.map { case k => s"${k.name}: ${prop(k).src}" }.mkString("{", ", ", "}")
      }"
    def src: String = messageString

    override def toString =
      s"($self)${
        customPropKeys.map { case k => s"${k.name}: ${prop(k)}" }.mkString("{", ", ", "}")
      }"
  }

  object Pred {
    def and(ps: Seq[Pred]): Pred = {
      require(ps.nonEmpty)
      ps.reduce(_ & _)
    }

    case class Default(tpe: TypeSym, self: Expr, propKeys: Set[PropKey], repo: PredRepo) extends Pred {
      override def prop(key: PropKey) = repo.defaultProp(tpe, key)
      override def customPropKeys = Set()
    }
    case class Custom(original: Pred, customSelf: Option[Expr], customProps: Map[PropKey, Pred]) extends Pred {
      override def tpe = original.tpe
      // TODO: check requirements
      override def self = customSelf getOrElse original.self
      override def prop(key: PropKey) = customProps.get(key) getOrElse original.prop(key)
      override def propKeys = original.propKeys
      override def customPropKeys = customProps.keySet
    }
    case class Substitute(original: Pred, mapping: Map[Value, Value]) extends Pred {
      override def tpe = original.tpe
      override def self = original.self.substitute(mapping)
      override def prop(key: PropKey) = original.prop(key).substitute(mapping)
      override def propKeys = original.propKeys
      override def customPropKeys = original.customPropKeys
    }
    case class Cast(original: Pred, tpe: TypeSym) extends Pred {
      // TODO: check requirements

      override def self = original.self
      override def prop(key: PropKey) = original.prop(key)
      override def propKeys = original.propKeys.filter(_.isTarget(tpe))
      override def customPropKeys = original.customPropKeys.filter(_.isTarget(tpe))
    }
  }

  sealed abstract class UnknownPred {
    def reveal(binding: Map[Value.Naked, Pred]): Pred = revealOpt(binding) getOrElse {
      throw new RuntimeException(s"Can't reveal $this(env=$binding)")
    }
    def revealOpt(binding: Map[Value.Naked, Pred]): Option[Pred]
    def dependency: Value
    def prop(k: PropKey): UnknownPred =
      UnknownPred.Ref(this, k)
    def substitute(mapping: Map[Value, Value]) =
      UnknownPred.Substitute(mapping, this)
    def tpe: TypeSym
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
      override def dependency = self.dependency
      override def toString = s"$self.$key"
      override def tpe = key.tpe
    }

    case class OfValue(value: Value) extends UnknownPred {
      override def revealOpt(binding: Map[Value.Naked, Pred]) = binding.get(value.naked)
      override def dependency = value
      override def toString = s"?($value)"
      override def tpe = value.tpe
    }

    case class Substitute(mapping: Map[Value, Value], original: UnknownPred) extends UnknownPred {
      override def revealOpt(binding: Map[Value.Naked, Pred]) =
        original.revealOpt(binding).map(_.substitute(mapping))
      override def dependency = original.dependency
      override def toString = s"[${mapping.toSeq.map { case (k, v) => s"$k -> $v" }.mkString(", ")}](${original.dependency})"
      override def tpe = original.tpe
    }
  }

  class PredRepo(world: World) {
    private[this] var defaults = Map.empty[TypeSym, Pred]

    def default(tpe: TypeSym): Pred = defaults.get(tpe) getOrElse {
      val pred = loadPred(tpe)
      defaults += (tpe -> pred)
      pred
    }

    def defaultProp(tpe: TypeSym, key: PropKey): Pred = {
      query.stableValueMembers(tpe).find { f => query.name(f) == key.name }.map { f =>
        val t = world.templates.get(f)
        t.bindings(t.ret)
      } getOrElse {
        throw new AssertionError(s"Can't find prop $key in $tpe")
      }
    }

    def custom(tpe: TypeSym, self: Option[Expr], props: Map[PropKey, Pred]): Pred = {
      // TODO: check requirements
      // TODO: check prop consistency(where?)
      Pred.Custom(default(tpe), self, props)
    }

    def compile(tpe: TypeSym, ast: Lang.Pred, env: Env): Pred = {
      custom(
        tpe,
        ast.self.map(Expr.compile(world, _, env, tpe)),
        ast.props.map {
          case (name, p) =>
            val key = world.findPropKey(name, tpe)
            key -> compile(key.tpe, p, env)
        })
    }

    def exactInt(v: Int): Pred =
      Pred.Custom(
        default(query.types.int),
        Some(CoreExpr.INT_EQ(CoreExpr.TheValue(query.types.int), CoreExpr.INT_Lit(v))),
        Map())

    def exactBoolean(v: Boolean): Pred =
      Pred.Custom(
        default(query.types.boolean),
        Some(CoreExpr.BOOL_EQ(CoreExpr.TheValue(query.types.boolean), CoreExpr.BOOL_Lit(v))),
        Map())

    private[this] def loadPred(tpe: TypeSym): Pred = {
      val keys = query.stableValueMembers(tpe).map { f =>
        world.findPropKey(query.name(f), tpe)
      }.toSet
      Pred.Default(tpe, CoreExpr.True, keys, this)
    }
  }
}
