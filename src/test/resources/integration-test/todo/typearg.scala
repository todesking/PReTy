// pending
// debugPrint
import com.todesking.prety.refine

class Wrap[A](val value: A)

object Typearg {
  def foo(): Unit = {
    @refine("_: {#1: _ == 1}")
    val a = new Wrap[Int](1)
  }
}
