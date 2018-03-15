// debugPrint
import com.todesking.prety.refine

object CoreMacros {
  @refine("_: @core.int.lt(_, 10)")
  val x1 = 9
  @refine("_: @core.int.lt(_, 10)")
  val x2 = 10
  //       ^ {_: _ == 10} *<= {_: @core.int.lt(_, 10)}
  @refine("_: @core.int.eq(_, 10)")
  val x3 = 10
  @refine("_: @core.int.eq(_, 10)")
  val x4 = 11
  //       ^ {_: _ == 11} *<= {_: @core.int.eq(_, 10)}
  @refine("_: @core.int.gt(_, 10)")
  val x5 = 11
  @refine("_: @core.int.gt(_, 10)")
  val x6 = 10
  //       ^ {_: _ == 10} *<= {_: @core.int.gt(_, 10)}
}
