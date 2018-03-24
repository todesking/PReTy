// debugPrint
import com.todesking.prety.refine

object Integer {
  @refine("lo: _ >= 0, hi: _ >= 0, _: _ >= 0")
  def mid(lo: Int, hi: Int): Int =
    (lo + hi) / 2 // may overflow
  //          ^ {_: @core.eq(_, @core.int.div(this, x))} *<= {_: _ >= 0}

  // prevent inlining
  @refine("_: _ == 2")
  val two = 2

  def foo(): Unit = {
    @refine("_: _ == 2")
    val x = 5 / two // OK
  }

  def bar(x: Int): Unit = {
    if(100 < x) {
      if(x < 112233) {
        @refine("_: _ > 0")
        val pos = x * 2
      } else {
        @refine("_: _ > 0")
        val pos = x * 2
        //          ^
      }
    }
  }
}
