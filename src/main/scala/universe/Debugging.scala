package com.todesking.prety.universe

trait Debugging { self: ForeignTypes =>
  def dprint(s: Any*): Unit =
    if (query.isDebugMode) {
      val content = s.mkString(" ")
      val lines = content.split("\n")
      if (lines.size > 1) {
        println("DEBUG:")
        println(lines.map("  " + _).mkString("\n"))
      } else {
        println("DEBUG: " + content)
      }
    }
  // for temporal debugging
  def ppp(s: Any*): Unit =
    dprint(s: _*)
}
