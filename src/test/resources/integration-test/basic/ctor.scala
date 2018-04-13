// debugPrint
import com.todesking.prety.refine
import refine.assert

class C0 {
}
class C1(val v: Int) {
}
trait T

object Ctor {
  def foo(): Unit = {
    assert(false)
    //     ^

    val c0 = new C0
    val c1 = new C1(1)

    @refine("_: {v: _ > 1}")
    val c2 = new C1(2)

    val c3 = new C1(3) with T

    assert(c1.v == 1)
    assert(c2.v > 1)
    assert(c3.v == 3)
  }
}
