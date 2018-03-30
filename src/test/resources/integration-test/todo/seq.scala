// pending
import com.todesking.prety.refine
import refine.assert

@refine.proxy("scala.Seq")
@refine.namespace("core.std.seq")
@refine.defineMemberPred("@Index: scala.Int", "_ >= 0 && _ < this.size")
@refine.defineMemberProp("@items: ItemsPred[A]")
@refine.defineMemberProp("@eq: EqualityPred")
trait Seq[A] {
  @refine("i: this.@Index, _: this.@eq(this.@items(i), _)")
  def apply(i: Int): A

  @refine.simple("_: _ >= 0")
  def size: Int
}

object SeqTest {
  def foo(): Unit = {
    assert(false)
    //     ^

    Seq(0, 1, 2)(-1)
    //           ^
    Seq(0, 1, 2)(3)
    //           ^

    assert(Seq(1, 2).size == 2)
    assert(Seq(1, 2).size == Seq(3, 4).size)
    assert(Seq(1, 2)(0) == 1)
    assert(Seq(1, 2)(1) == 2)
    assert(Seq(1, 2) == Seq(1, 2))
    assert(Seq(1, 2) != Seq(3, 4))
  }

  @refine("xs: @Seq(_ == 1, _ == 2, _ > 3)")
  def bar(xs: Seq[Int]): Unit = {
  }
}
