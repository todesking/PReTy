package com.todesking.prety.scalac_plugin

import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }

class PretyPlugin(override val global: Global) extends Plugin {
  override val name = "prety-scalac-plugin"
  override val description = "PReTy Scalac Plugin"
  override val components = List(new Component(global))
}

class Component(override val global: Global) extends PluginComponent {
  override val phaseName = "prety-check-phase"
  override val runsAfter = List("typer")
  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
    override def apply(unit: global.CompilationUnit): Unit = {
      ???
    }
  }
}
