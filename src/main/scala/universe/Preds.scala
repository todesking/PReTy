package com.todesking.prety.universe

trait Preds { self: ForeignTypes with Values with Props with Envs with Exprs with Conflicts with Worlds with Templates with Debugging =>
  // TODO: Expr -> Expr.Boolean
  abstract class Pred {
    def tpe: TypeSym

    def self: Expr

    // where this.tpe <:< key.targetType
    // where pred.tpe <:< key.tpe
    // TODO: check requirements of key
    def prop(key: PropKey): Pred
    def prop(path: Seq[PropKey]): Pred =
      path.foldLeft(this) { (pred, p) => pred.prop(p) }

    def propKeys: Set[PropKey]
    def customPropKeys: Set[PropKey]
    def customized(k: PropKey): Boolean =
      customPropKeys.contains(k)

    def customSelf: Option[Expr]

    // TODO: check requirements
    // TODO: check prop consistency(where?)
    def custom(self: Expr): Pred.Custom =
      custom(Some(self), Map())

    // TODO: self: Expr
    def custom(self: Option[Expr], props: Map[PropKey, Pred]): Pred.Custom =
      Pred.Custom(this, self, props)

    def custom(path: Seq[PropKey], pred: Pred): Pred = {
      if (path.isEmpty) {
        require(pred.tpe == this.tpe)
        val cs = if (pred.self == this.self) None else Some(pred.self)
        custom(cs, pred.customPropKeys.map { k => k -> pred.prop(k) }.toMap)
      } else {
        val k = path.head
        custom(None, Map(k -> prop(k).custom(path.tail, pred)))
      }
    }

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
        (this.customPropKeys ++ rhs.customPropKeys).toSeq
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

    case class Default(tpe: TypeSym, self: Expr, propKeys: Set[PropKey], defaultPred: PropKey => Pred) extends Pred {
      override def customSelf = None
      override def prop(key: PropKey) = defaultPred(key)
      override def customPropKeys = Set()
    }
    // TODO: AND to default pred
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
      override def customSelf = original.customSelf.map(_.substitute(mapping))
      override def prop(key: PropKey) = original.prop(key).substitute(mapping)
      override def propKeys = original.propKeys
      override def customPropKeys = original.customPropKeys
    }
    // TODO: Do I really need it?
    case class Cast(original: Pred, tpe: TypeSym) extends Pred {
      // TODO: check requirements

      override def customSelf = original.customSelf
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
    def dependencies: Set[Value]
    def tpe: TypeSym
    def &(rhs: UnknownPred): UnknownPred =
      UnknownPred.And(this, rhs)
    def prop(k: PropKey): UnknownPred =
      UnknownPred.Ref(this, k)
    def substitute(mapping: Map[Value, Value]): UnknownPred =
      UnknownPred.Substitute(mapping, this)
  }
  object UnknownPred {
    def ref(v: Value, k: PropKey): UnknownPred =
      OfValue(v).prop(k)

    case class And(lhs: UnknownPred, rhs: UnknownPred) extends UnknownPred {
      require(lhs.tpe <:< rhs.tpe)

      override def revealOpt(binding: Map[Value.Naked, Pred]) =
        for {
          l <- lhs.revealOpt(binding)
          r <- rhs.revealOpt(binding)
        } yield l & r
      override def dependencies = lhs.dependencies ++ rhs.dependencies
      override def toString = s"$lhs & $rhs"
      override def tpe = rhs.tpe
    }

    case class Ref(self: UnknownPred, key: PropKey) extends UnknownPred {
      override def revealOpt(binding: Map[Value.Naked, Pred]) =
        self.revealOpt(binding)
          .map { pred =>
            pred.prop(key)
          }
      override def dependencies = self.dependencies
      override def toString = s"$self.${key.name}"
      override def tpe = key.tpe
    }

    case class OfValue(value: Value) extends UnknownPred {
      override def revealOpt(binding: Map[Value.Naked, Pred]) = binding.get(value.naked)
      override def dependencies = Set(value)
      override def toString = s"?${value.shortString}"
      override def tpe = value.tpe
    }

    case class Substitute(mapping: Map[Value, Value], original: UnknownPred) extends UnknownPred {
      override def revealOpt(binding: Map[Value.Naked, Pred]) =
        original.revealOpt(binding).map(_.substitute(mapping))
      override def dependencies = original.dependencies // TODO: refer mapping.values & original.dependencies
      override def toString = s"$original[${mapping.toSeq.map { case (k, v) => s"${k.shortString} -> ${v.shortString}" }.mkString(", ")}]"
      override def tpe = original.tpe
    }
  }

}
