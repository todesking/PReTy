package com.todesking.prety.universe

trait Universe extends AnyRef
  with ForeignTypes
  with Constraints
  with Conflicts
  with Values
  with ASTs
  with Preds
  with Macros
  with Worlds
  with Envs
  with Exprs
  with Props
  with Graphs
  with Templates
  with Solvers
  with TypeCheckers
  with Debugging {

  def toAST(world: World, t: Tree): Seq[AST.CTODef]
  def reportError(pos: Pos, msg: String): Unit

  def checkRefinements(root: Tree): Unit = {
    val world = World.buildDefault()
    def pos(v: Value) = world.values.getPos(v) match {
      case Some(p) =>
        s"${query.lineNum(p)}:${query.columnNum(p)}"
      case None =>
        s"???"
    }

    val ctos = toAST(world, root)
    val checker = new TypeChecker(world)
    ctos.foreach { cto =>
      val conflicts = checker.check(cto)

      conflicts.foreach { c =>
        dprint(s"CONFLICT at ${pos(c.focus)}: ${c.message}")
        val p = world.values.getPos(c.focus) getOrElse query.emptyPos
        reportError(p, c.message)
      }
    }
  }
}

