
import com.todesking.prety.refine
import refine.assert

object Assert {
  def foo(x: Int): Unit = {
    assert(false)
    //     ^
    assert(true)

    assert(x == 1)
    //       ^
    assert(1 == 1)
    if(x == 1)
      assert(x == 1)
  }
}
