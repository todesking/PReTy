// pending

import com.todesking.prety.refine

// Example from "Programming with Refinement Types"( http://goto.ucsd.edu/~rjhala/lh-book-draft.pdf )

@refine.proxy("scala.math.Ordering")
@refine.defineMemberUF("@gt(l: A, r: A): Boolean")
@refine.defineMemberUF("@eq(l: A, r: A): Boolean")
@refine.axiom("{a: A}; @eq(a, a) == true")
@refine.axiom("{a: A, b: A}; @eq(a, b) --> @eq(b, a)")
@refine.axiom("{a: A, b: A, c: A}; @eq(a, b) && @eq(b, c) --> @eq(a, c)")
@refine.axiom("{a: A, b: A, c: A}; @gt(a, b) && @gt(b, c) --> @gt(a, c)")
@refine.axiom("{a: A, b: A}; @gt(a, b) --> !@gt(b, a)")
@refine.axiom("{a: A, b: A}; @gt(a, a) == false")
trait OrderingProxy[A] {
  @refine.measure("@gt(l, r) || @eq(l, r)")
  def >=(l: A, r: A): Boolean
  @refine.measure("@gt(l, r)")
  def >(l: A, r: A): Boolean
}

sealed abstract class IncList[A] {
  @refine("this: {nonEmpty}")
  def ordering: Ordering[A]

  @refine("this: {nonEmpty}")
  def hd: A
  @refine("this: {nonEmpty}, _: {#A: ordering.>(_, hd)}")
  def tl: IncList[A]

  val isEmpty: Boolean // stable values treated as measure automatically
  @refine.measure("!this.isEmpty")
  def nonEmpty: Boolean
}

@refine("_: {isEmpty, #A: @False}")
case object Emp extends IncList[Nothing] {
  override def ordering = ???
  override def hd = ???
  override def tl = ???

  @refine("_: _ == true")
  override def isEmpty = true
}

@refine.refine("tl: {#A: ordering.>(_, hd)}}, _: {nonEmpty, #A: ordering.>=(_, hd)}")
case class <::<[A](hd: A, tl: IncList[A])(implicit val ordering: Ordering[A]) extends IncList[A] {
  @refine("_: _ == false")
  override def isEmpty = false
}

object Test {

  @refine("_: b")
  def staticAssert(b: Boolean) = ()
  def foo(): Unit = {
    val a = Emp[Int]()

    a.hd
    //^ {false}
    a.tl
    //^ {false}

    val b = 1 <:< Emp()

    b.hd
    b.tl
    b.tl.hd
    //   ^ {false}
    b.tl.tl
    //   ^ {#A > hd} where hd: {#A: false}

    val c = 1 <:< 2<:< Emp()
    staticAssert(c.hd < c.tl.hd)

    val d = 2 <:< 1 <:< Emp()
    //      ^ {_ == 2} *<= {_: ev.<=(_, #A)}
  }
}
