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
    //      ^

    lt(1, 2)
    lt(2, 2)
    // ^

    lt(x, y)
    lt(x, x)
    // ^
    lt(y, y)
    // ^
    lt(y, x)
    // ^
  }

  def bar(x: Int): Unit = {
    lt(x, 3)
    // ^
    if(x > 3) {
      lt(x, 3)
      // ^
      // TODO: show lhs as {_: _ > 3}
      lt(3, x)
    } else {
      lt(x, 4)
      lt(4, x)
      // ^

      if(x > 1) { // x = {2, 3}
        lt(x, 4)
        lt(x, 3)
        // ^
        lt(x, 2)
        // ^
      }
    }
  }

  def baz(x: Int): Unit = {
    val y = 10
    if(x < y) {
      lt(x, y)
    }
    lt(x, y)
    // ^
    if(lessThan(x, y)) {
      lt(x, y)
    }
    lt(x, y)
    // ^
  }

  // TODO: fix private member refinement inference
  @refine("_: _ == (x < y)")
  private[this] def lessThan(x: Int, y: Int): Boolean =
    x < y
}
