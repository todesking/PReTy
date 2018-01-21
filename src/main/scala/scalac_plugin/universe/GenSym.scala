package com.todesking.prety.scalac_plugin.universe

class GenSym[A] {
  private[this] var _next = 0
  private[this] def genNextId(): Int = {
    _next += 1
    _next - 1
  }
  def apply(): Int = genNextId()
}

