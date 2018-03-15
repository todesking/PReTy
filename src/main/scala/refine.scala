package com.todesking.prety

import scala.annotation.StaticAnnotation

class refine(src: String) extends StaticAnnotation

object refine {
  class proxy(forClass: String) extends StaticAnnotation
}
