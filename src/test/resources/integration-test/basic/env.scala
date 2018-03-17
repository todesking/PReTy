// debugPrint
import com.todesking.prety.refine

object Env {
  @refine("x: _ < y")
  def lt(x: Int, y: Int): Unit = ()

  def foo(): Unit = {
    val x = 1

    @refine("_: _ > x")
    val y = 2

    @refine("_: _ == x")
    val z = y
    //      ^ {_: _ > x} *<= {_: _ == x}

    lt(1, 2)
    lt(2, 2)
    // ^ {_: _ == 2} *<= {_: _ < y}

    lt(x, y)
    lt(x, x)
    // ^ {_: _ == 1} *<= {_: _ < y}
    lt(y, y)
    // ^ {_: _ > x} *<= {_: _ < y}
    lt(y, x)
    // ^ {_: _ > x} *<= {_: _ < y}
  }

  def bar(x: Int): Unit = {
    lt(x, 3)
    // ^ {} *<= {_: _ < y}
    if(x > 3) {
      lt(x, 3)
      // ^ {} *<= {_: _ < y}
      // TODO: show lhs as {_: _ > 3}
      lt(3, x)
    } else {
      lt(x, 4)
      lt(4, x)
      // ^ {_: _ == 4} *<= {_: _ < y}

      if(x > 1) { // x = {2, 3}
        lt(x, 4)
        lt(x, 3)
        // ^ {} *<= {_: _ < y}
        lt(x, 2)
        // ^ {} *<= {_: _ < y}
      }
    }
  }
}
