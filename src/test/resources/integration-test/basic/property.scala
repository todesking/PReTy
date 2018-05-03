// debugPrint

import com.todesking.prety.refine

trait A {
  @refine("_: _ > 0")
  val value: Int
  val a: A
}

object InstanceRefinement {
  @refine("a: {a: {value: _ == 3}}")
  def f1(a: A): Unit = {
    refine.assert(a.value > 0)
    refine.assert(a.a.value == 3)
  }
  // TODO: force {value: _ < 10 && _ > 0} explicitly
  @refine("a: {value: _ < 10}")
  def f2(a: A): Unit = {
    // refine.assert(a.value > 0)
    refine.assert(a.value < 10)
  }
}
