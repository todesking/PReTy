// pending
// debugPrint

import com.todesking.prety.refine

trait A {
  @refine("_: _ > 0")
  val value: Int
}

object InstanceRefinement {
  def f1(a: A): Unit = {
    refine.assert(a.value > 0)
  }
  @refine("a: {value: _ < 10}")
  def f2(a: A): Unit = {
    refine.assert(a.value > 0)
    refine.assert(a.value < 10)
  }
}
