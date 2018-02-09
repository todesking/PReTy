package com.todesking.prety.scalac_plugin.universe

import com.todesking.prety.Template

import com.todesking.prety.{ Pred, Graph, Solver, Lang, Value }

trait Universe extends AnyRef
  with ForeignTypes
  with ASTs {
  private[this] def unk(t: AST): Nothing =
    throw new RuntimeException(s"Unknown AST: $t")

  def checkRefinements(root: Tree): Unit = {
    val ctos = toAST(root)
    ctos.foreach(analyzeCTO)
  }

  def analyzeCTO(cto: AST.CTODef): Unit = {
    println(s"Analyzing CTO: ${cto.pretty.toString(0)}")
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
    println("Unbound:")
    (inferred.allValues -- inferred.binding.keySet).toSeq
      .sortBy(_.id)
      .foreach {
        case v =>
          println(s"  $v")
      }

    val conflicts = Solver.solve(inferred)
    conflicts.foreach { c =>
      println(s"CONFLICT: ${"???"}: ${c.message}")
      reportError(null, c.message)
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
      // TODO: register template.binding (or make global binding repo) for foreign members
      val template = templateOf(sym)
      buildGraph(self)
        .merge(argss.flatten.map(buildGraph))
        .merge(template.apply(self.value, value, argss.map(_.map(_.value))))

    case AST.ValRef(sym, tpe, value) =>
      // TODO: add equality
      Graph.empty

    case AST.Super(tpe, value) =>
      // TODO: we can do something here
      Graph.empty

    case AST.IntLiteral(value, lit) =>
      Graph.bind(
        value,
        Pred.Expr(
          Lang.AST.Op(Lang.AST.TheValue, "==", Lang.AST.LitInt(lit)),
          Map()))

    case AST.UnitLiteral(value) =>
      Graph.bind(value, Pred.True)
  }

  class ValueRepo {
    private[this] var values = Map.empty[DefSym, Value]
    private[this] var thisValues = Map.empty[DefSym, Value]
    private[this] var positions = Map.empty[Value, Pos]
    private[this] var nextValueId = 0

    private[this] def register(key: DefSym, name: String): Value = {
      if (values.contains(key))
        throw new RuntimeException(s"Value conflict: $key")
      val v = fresh(name)
      values = values + (key -> v)
      v
    }

    def posOf(v: Value): Option[Pos] =
      positions.get(v)

    def fresh(name: String): Value = {
      val v = Value(nextValueId, name)
      nextValueId += 1
      v
    }
    def registerParam(fun: DefSym, p: DefSym): Value =
      register(p, s"${query.name(fun)}/(${query.name(p)})")

    def getOrRegisterReturn(fun: DefSym): Value =
      values.get(fun).getOrElse {
        register(fun, s"${query.name(fun)}/return")
      }

    def getOrRegisterThis(fun: DefSym): Value =
      thisValues.get(fun).getOrElse {
        val v = fresh(s"${query.name(fun)}/this")
        thisValues = thisValues + (fun -> v)
        v
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
    val self = valueRepo.getOrRegisterThis(f)
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

  def reportError(pos: Pos, msg: String): Unit

}

