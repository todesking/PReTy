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
      // TODO: Use default binding if public
      val template = templateOf(sym)
      body.fold(Graph.empty) { b =>
        Graph
          .constraint(b.value *<:= value)
          .merge(buildGraph(b))
          .constraint(b.value *<:= template.ret)
      }.bind(template.bindings)
    case AST.FunDef(sym, tpe, body) =>
      // TODO: Use default binding if public
      val template = templateOf(sym)
      body.fold(Graph.empty) { b =>
        Graph.constraint(b.value *<:= template.ret)
          .merge(buildGraph(b))
      }.bind(template.bindings)
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
        .merge(template.apply(self.value, value, argss.map(_.map(_.value))))
    case AST.ValRef(sym, tpe, value) =>
      // TODO: add equality
      Graph.empty
    case AST.Super(tpe, value) =>
      // TODO: we can do something here
      Graph.empty
    case AST.Select(tpe, value, target, sym) =>
      // TODO: lookup bindings from template
      val template = templateOf(sym)
      buildGraph(target)
        .merge(template.apply(target.value, value, Seq()))
    case AST.IntLiteral(value, lit) =>
      Graph.bind(
        value,
        Pred.Expr(
          Lang.AST.Op(Lang.AST.TheValue, "==", Lang.AST.LitInt(lit)),
          Map()))
    case AST.UnitLiteral(value) =>
      Graph.empty
  }

  class ValueRepo {
    private[this] var values = Map.empty[DefSym, Value]
    private[this] def register(key: DefSym, name: String): Value = {
      if (values.contains(key))
        throw new RuntimeException(s"Value conflict: $key")
      val v = Value.fresh(name)
      values = values + (key -> v)
      v
    }

    def registerParam(fun: DefSym, p: DefSym): Value =
      register(p, s"${query.name(fun)}/(${query.name(p)})")

    def getOrRegisterReturn(fun: DefSym): Value =
      values.get(fun).getOrElse {
        register(fun, s"${query.name(fun)}")
      }

    def get(key: DefSym): Value =
      values.get(key).getOrElse {
        throw new RuntimeException(s"Undefined value: $key")
      }
  }

  val valueRepo = new ValueRepo

  private[this] var templates = Map.empty[DefSym, Template]
  def templateOf(f: DefSym): Template = {
    templates.get(f).getOrElse {
      val t = freshTemplate(f)
      this.templates = templates + (f -> t)
      t
    }
  }

  private[this] def freshTemplate(f: DefSym): Template = {
    val srcs = query.refinementSrc(f)
    val asts = srcs.flatMap(Lang.parse)
    buildTemplate(f, asts)
  }

  private[this] def buildTemplate(f: DefSym, preds: Seq[(String, Lang.AST)]): Template = {
    // TODO: Check unknown pred target
    val fname = query.name(f)
    val self = Value.fresh(s"$fname/this")
    val paramss = query.paramss(f)
      .map { ps => ps.map { p => valueRepo.registerParam(f, p) } }
    // When f is local val, ret is already registered as param
    val ret = valueRepo.getOrRegisterReturn(f)

    val env = query.paramss(f).zip(paramss).flatMap {
      case (ps, vs) =>
        ps.zip(vs).map { case (p, v) => query.name(p) -> v }
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
      aArgss: Seq[Seq[Value]]): Graph = {
      require(argss.map(_.size) == aArgss.map(_.size))

      val argSub = Map(self -> aSelf) ++
        argss.flatten.zip(aArgss.flatten).map { case (p, a) => p -> a }

      Graph
        .constraint(aSelf *<:= self)
        .constraint(
          argss.flatten.zip(aArgss.flatten).map {
            case (p, a) =>
              a *<:= p.substitute(argSub)
          }).constraint(aRet *<:= ret.substitute(argSub))
        .bind(bindings)
    }
  }

  case class BoundValue(value: Value, pred: Pred)

  def solve(cs: Seq[Constraint], binding: Map[Value, Pred]): Seq[Conflict] = {
    Seq()
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
    // TODO: check conflict
    def bind(vps: Map[Value, Pred]) =
      new Graph(constraints, binding ++ vps)

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

