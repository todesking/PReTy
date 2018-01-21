// pending
import com.todesking.prety.refine

object Parsing {
  @refine("_: _ > 0")
  val x = 1

  @refine("")
  //       ^
  val y = 1
}
