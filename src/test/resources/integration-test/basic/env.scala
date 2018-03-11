// debugPrint
import com.todesking.prety.refine

object Env {
  @refine("x: _ < y")
  def lt(x: Int, y: Int): Unit = ???

  def foo(): Unit = {
    val x = 1

    @refine("_: _ > x")
    val y = 2


    lt(1, 2)
    lt(2, 2)
    // ^ {_ == 2} *<= {_ < y}

    lt(x, y)
    lt(x, x)
    // ^ {_ == 1} *<= {_ < y}
    lt(y, y)
    // ^ {_ > x} *<= {_ < y}
    lt(y, x)
    // ^ {_ > x} *<= {_ < y}
  }
}
