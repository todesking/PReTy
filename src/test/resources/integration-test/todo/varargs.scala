// pending
import com.todesking.prety.refine

object VarArgs {
  @refine("_: @core.seq.eq(xs, _)}")
  def va(xs: Int*): Seq[Int] =
    xs

  def foo(): Unit = {
    @refine("_ == 1")
    val one = va(1, 2, 3)(0)

    @refine("_ == 2")
    val two = va(1, 2, 3)(2)
    //                   ^
  }
}
