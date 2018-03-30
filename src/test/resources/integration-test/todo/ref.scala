// pending
// debugPrint
import com.todesking.prety.refine

object Nullable {
  @refine("s: {@core.prop.ref: _ ne null}")
  def foo(x: String): Unit = {
  }

  def f(x: String): Unit = {
    foo("aaa")
    foo(null)
    //  ^ {@core.prop.ref: _ eq null} *<= {@core.prop.ref: _ ne null}
    foo(x)
    //  ^ {@core.prop.ref: _ eq null} *<= {@core.prop.ref: _ ne null}
    if(x ne null) {
      foo(x)
    }
  }
}
