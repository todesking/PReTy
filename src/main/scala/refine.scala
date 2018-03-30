package com.todesking.prety

import scala.annotation.StaticAnnotation

class refine(src: String) extends StaticAnnotation

object refine {
  class proxy(forClass: String) extends StaticAnnotation
  // Annotate a method as "simple"
  // Simple method is:
  // - No refinements other than retval
  // - No side effects
  // - Result type is Int or Boolean
  // When calling simple method in Scala, treated as _: {_: _ == expr}
  // When calling from refinement spec, substitute(expr, params -> args) are inserted.
  class simple(expr: String) extends StaticAnnotation

  @refine("cond: _ == true")
  def assert(cond: Boolean): Unit = {
    Predef.assert(cond)
  }
}
