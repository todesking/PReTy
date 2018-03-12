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
}
