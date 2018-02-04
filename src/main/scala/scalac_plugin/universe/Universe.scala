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
    val graph = Graph.merge(cto.impl.map(buildGraph))
    val inferred = graph.infer()
    println(inferred.constraints.mkString("\n"))
    println(s"Initial:")
    graph.binding.toSeq.sortBy(_._1.id)
      .foreach {
        case (v, p) =>
          println(s"  $v = $p")
      }
    println(s"Inferred:")
    (inferred.binding.keySet -- graph.binding.keySet).toSeq
      .sortBy(_.id)
      .foreach {
        case v =>
          println(s"  $v = ${inferred.binding(v)}")
      }

    val conflicts = solve(inferred.constraints, inferred.binding)
    conflicts.foreach { c =>
      reportError(c.pos, c.message)
    }
  }

  def buildGraph(t: AST.InImpl): Graph = t match {
    case AST.CTODef(impl) => unk(t)
    case AST.ValDef(sym, tpe, value, body) =>
      // TODO: check val's annotation
      // TODO: Use default binding if public
      body.fold(Graph.empty) { b =>
        Graph.constraint(b.value *<:= value) + buildGraph(b)
      }
    case AST.FunDef(sym, tpe, value, paramss, body) =>
      // TODO: Use default binding if public
      // TODO: gather bindings from definition
      body.fold(Graph.empty) { b =>
        Graph.constraint(b.value *<:= value) + buildGraph(b)
      }
    case AST.Block(tpe, value, stats, expr) =>
      Graph.merge(stats.map(buildGraph))
        .merge(buildGraph(expr))
        .constraint(expr.value *<:= value)
    case AST.This(tpe, value) =>
      Graph.bind(value, Pred.True)
    case AST.Apply(self, sym, tpe, value, argss) =>
      val template = templateOf(sym)
      // TODO: lookup value's binding from sym's template
      buildGraph(self)
        .merge(argss.flatten.map(buildGraph))
        .constraint(template.apply(self.value, value, argss.map(_.map(_.value))))
    case AST.ValRef(sym, tpe, value) =>
      // TODO: add equality
      Graph.empty
    case AST.Super(tpe, value) =>
      // TODO: we can do something here
      Graph.empty
    case AST.Select(tpe, value, target, sym) =>
      // TODO: lookup bindings from template
      buildGraph(target)
        .constraint(target.value *<:= value)
    case AST.IntLiteral(value, lit) =>
      Graph.bind(
        value,
        Pred.Expr(
          Lang.AST.Op(Lang.AST.TheValue, "==", Lang.AST.LitInt(lit)),
          Map()))
    case AST.UnitLiteral(value) =>
      Graph.empty
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
      Map(value -> Pred.True)
    case AST.Apply(self, sym, tpe, value, argss) =>
      val template = templateOf(sym)
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
    val inferred = g.infer()
    println(s"Initially assigned: ${initialValues}")
    println(s"inferred: ${g.binding}")
    if (inferred.unassignedValues.nonEmpty) {
      throw new RuntimeException(
        s"Infer failed: Unassigned=${inferred.unassignedValues}")
    }
    g.binding
  }

  class Graph(
    val constraints: Seq[Constraint],
    val binding: Map[Value, Pred]) {
    def +(rhs: Graph) = new Graph(
      constraints ++ rhs.constraints,
      binding ++ rhs.binding)

    def merge(rhs: Graph) = this + rhs
    def merge(rhs: Seq[Graph]) = this + Graph.merge(rhs)

    def constraint(c: Constraint) =
      this + Graph.constraint(c)
    def constraint(cs: Seq[Constraint]) =
      this + Graph.constraint(cs)

    lazy val allValues = constraints.flatMap(_.values).toSet
    lazy val assignedValues = binding.keySet
    lazy val unassignedValues = allValues -- assignedValues

    def hasUnassignedIncomingEdge(v: Value): Boolean =
      incomingEdges(v).flatMap(_.values).exists(unassignedValues)

    def incomingEdges(v: Value): Set[Constraint] =
      constraints.filter { c => c.lhs.toValue.contains(v) }.toSet

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
      new Graph(constraints, newBinding)
    }
  }
  object Graph {
    val empty: Graph = new Graph(Seq(), Map())

    def merge(gs: Seq[Graph]): Graph =
      gs.foldLeft(empty) { (a, x) => a + x }
    def constraint(c: Constraint): Graph =
      new Graph(Seq(c), Map())
    def constraint(cs: Seq[Constraint]): Graph =
      new Graph(cs, Map())
    def bind(v: Value, p: Pred): Graph =
      new Graph(Seq(), Map(v -> p))
  }
  class Env {
    def isVisibleFrom(v: Value, from: Value): Boolean = ???
  }

  def reportError(pos: Pos, msg: String): Unit

  case class Conflict(pos: Pos, message: String)
}

