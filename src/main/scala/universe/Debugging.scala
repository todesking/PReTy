package com.todesking.prety.universe

trait Debugging { self: Queries =>
  def dprint(s: String): Unit =
    if (query.isDebugMode) println(s)
}
