package com.todesking.prety.scalac_plugin.universe

trait Universe extends AnyRef
  with ForeignTypes
  with ASTs
  with Preds
  with Constraints
  with Inferences {
  private[this] def unk(t: AST): Nothing =
    throw new RuntimeException(s"Unknown AST: $t")

  def checkRefinements(root: Tree): Unit = {
    val ctos = toAST(root)
    ctos.foreach(analyzeCTO)
  }

  def analyzeCTO(cto: AST.CTODef): Unit = {
    println(s"Analyzing: $cto")
    val constraints = cto.impl.flatMap(gatherConstraints)
    val initialBinding = cto.impl.flatMap(gatherPredBindings).toMap // TODO: Check dups
    val binding = infer(constraints, initialBinding)
    val conflicts = solve(constraints, binding)
    conflicts.foreach { c =>
      reportError(c.pos, c.message)
    }
  }

  def gatherPredBindings(t: AST.InImpl): Map[Value, Pred] = t match {
    case AST.CTODef(impl) => unk(t)
    case AST.ValDef(sym, tpe, value, body) =>
      // TODO: check val's annotation
      // TODO: Use default binding if public
      body.map(gatherPredBindings).getOrElse(Map())
    case AST.FunDef(sym, tpe, value, paramss, body) =>
      // TODO: Use default binding if public
      // TODO: gather bindings from definition
      body.map(gatherPredBindings).getOrElse(Map())
    case AST.Block(tpe, value, stats, expr) =>
      stats.flatMap(gatherPredBindings).toMap ++ gatherPredBindings(expr)
    case AST.This(tpe, value) =>
      // TODO: we can do something here
      Map()
    case AST.Apply(self, sym, tpe, value, argss) =>
      // TODO: lookup value's binding from sym's template
      gatherPredBindings(self) ++ argss.flatten.flatMap(gatherPredBindings)
    case AST.ValRef(sym, tpe, value) =>
      // TODO: add equality
      Map()
    case AST.Super(tpe, value) =>
      // TODO: we can do something here
      Map()
    case AST.Select(tpe, value, target, sym) =>
      // TODO: lookup bindings from template
      gatherPredBindings(target)
    case AST.IntLiteral(value, lit) =>
      Map(
        value -> Pred.Expr(
          Lang.AST.Op(Lang.AST.TheValue, "==", Lang.AST.LitInt(lit)),
          Map()))
    case AST.UnitLiteral(value) =>
      Map()
  }

  def gatherConstraints(t: AST.InImpl): Seq[Constraint] = t match {
    case AST.CTODef(impl) => unk(t)
    case AST.ValDef(sym, tpe, value, body) =>
      body.fold(Seq.empty[Constraint]) { b =>
        Seq(b.value *<:= value) ++ exprConstraints(b)
      }
    case AST.FunDef(sym, tpe, value, paramss, body) =>
      body.fold(Seq.empty[Constraint]) { b =>
        Seq(b.value *<:= value) ++ exprConstraints(b)
      }
    case e: AST.Expr =>
      exprConstraints(e)
  }

  def exprConstraints(e: AST.Expr): Seq[Constraint] = e match {
    case AST.Block(tpe, value, stats, expr) =>
      stats.flatMap(gatherConstraints) ++ exprConstraints(expr)
    case AST.IntLiteral(value, lit) =>
      Seq()
    case AST.UnitLiteral(value) =>
      Seq()
    case AST.This(tpe, value) =>
      Seq()
    case AST.ValRef(sym, tye, value) =>
      Seq()
    case AST.Apply(self, sym, tpe, value, argss) =>
      val selfCs = exprConstraints(self)
      val argCs = argss.flatMap(_.flatMap(exprConstraints))
      val applyCs = templateOf(sym)
        .apply(self.value, value, argss.map(_.map(_.value)))
      selfCs ++ argCs ++ applyCs
    case AST.Super(tpe, value) =>
      Seq()
    case AST.Select(tpe, value, target, sym) =>
      Seq()
  }

  private[this] var templates = Map.empty[FunSym, Template]
  def templateOf(f: FunSym): Template = {
    templates.get(f).fold {
      val t = freshTemplate(f)
      this.templates = templates + (f -> t)
      t
    }(identity)
  }

  private[this] def freshTemplate(f: FunSym): Template = {
    val srcs = refinementSrcFromFun(f)
    val asts = srcs.flatMap(Lang.parse)
    buildTemplate(f, asts)
  }

  private[this] def buildTemplate(f: FunSym, preds: Seq[(String, Lang.AST)]): Template = {
    val fname = funName(f)
    val self = Value.fresh(s"$fname/<this>")
    val paramss = funParamNames(f).map { ns => ns.map { name => Value.fresh(s"$fname/param:$name") } }
    val ret = Value.fresh(s"$fname/return")

    val env = funParamNames(f).zip(paramss).flatMap {
      case (ns, vs) =>
        ns.zip(vs)
    }.toMap ++ Map("this" -> self, "_" -> ret)

    val bindings = preds
      .groupBy(_._1)
      .toMap
      .mapValues(_.map(_._2))
      .mapValues { asts =>
        Pred.and(asts.map(Pred.Expr(_, env)))
      }.map {
        case (k, v) =>
          env(k) -> v
      }
    Template(self, ret, paramss, bindings)
  }

  case class Template(
    self: Value,
    ret: Value,
    argss: Seq[Seq[Value]],
    bindings: Map[Value, Pred]) {
    // TODO: check acyclic
    def apply(
      aSelf: Value,
      aRet: Value,
      aArgss: Seq[Seq[Value]]): Seq[Constraint] = {
      require(argss.map(_.size) == aArgss.map(_.size))

      val argSub = Map(self -> aSelf) ++
        argss.flatten.zip(aArgss.flatten).map { case (p, a) => p -> a }

      return (
        (aSelf *<:= self) +: argss.flatten.zip(aArgss.flatten).map {
          case (p, a) =>
            a *<:= p.substitute(argSub)
        } :+ (aRet *<:= ret.substitute(argSub))) ++ bindings.map { case (v, p) => v *=:= p }
    }
  }

  case class BoundValue(value: Value, pred: Pred)

  def solve(cs: Seq[Constraint], binding: Map[Value, Pred]): Seq[Conflict] = {
    Seq()
  }
  def infer(
    constraints: Seq[Constraint],
    initialValues: Map[Value, Pred]): Map[Value, Pred] = {
    println(constraints.mkString("\n"))
    val g = new Graph(constraints, initialValues)
    val infered = g.infer()
    println(s"Initially assigned: ${initialValues}")
    println(s"Infered: ${g.binding}")
    if (infered.unassignedValues.nonEmpty) {
      throw new RuntimeException(
        s"Infer failed: Unassigned=${infered.unassignedValues}")
    }
    g.binding
  }

  class Graph(cs: Seq[Constraint], val binding: Map[Value, Pred]) {
    lazy val allValues = cs.flatMap(_.values).toSet
    lazy val assignedValues = binding.keySet
    lazy val unassignedValues = allValues -- assignedValues

    def hasUnassignedIncomingEdge(v: Value): Boolean =
      incomingEdges(v).flatMap(_.values).exists(unassignedValues)

    def incomingEdges(v: Value): Set[Constraint] =
      cs.filter { c => c.lhs.toValue.contains(v) }.toSet

    @scala.annotation.tailrec
    final def infer(): Graph = {
      val next = infer0()
      if (next.binding == this.binding) this
      else next.infer()
    }

    private[this] final def infer0(): Graph = {
      // TODO: weaken with visibility
      val newBinding =
        unassignedValues
          .filterNot(hasUnassignedIncomingEdge)
          .foldLeft(binding) { (b, v) =>
            val p = Pred.and(incomingEdges(v).map(_.lhs).map(_.pred(b)).toSeq)
            b + (v -> p)
          }
      new Graph(cs, newBinding)
    }
  }
  class Env {
    def isVisibleFrom(v: Value, from: Value): Boolean = ???
  }

  def reportError(pos: Pos, msg: String): Unit

  case class Conflict(pos: Pos, message: String)
}

