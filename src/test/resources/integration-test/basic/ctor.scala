// debugPrint
import com.todesking.prety.refine
import refine.assert

class C @refine("_: {v: _ == x}") (x: Int) {
  val v: Int = x
}

// TODO
// @refine("v: _ > 0")
// class C2 @refine("v: _ < 10") (@refine("v != 2") val v: Int) {
// }


abstract class  A {
  val v: Int = 0
}

object Ctor {
  @refine("c: {v: _ == 100}")
  def a(c: C): Unit = {
    assert(c.v == 100)
  }
  @refine("c: {v: _ == 100}")
  def b(c: A): Unit = {
    assert(c.v == 100)
  }
  def foo(): Unit = {
    @refine("_: {v: _ == 1}")
    val c1 = new C(1)
    assert(c1.v == 1)

    // TODO: with
    // val c3 = new C1(3) with T

    // TODO: anon class
    // val c4 = new C1(0) {
    //   override val v = 4
    // }
  }
}
