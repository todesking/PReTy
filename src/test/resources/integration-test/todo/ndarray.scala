// pending

import com.todesking.prety.refine

sealed abstract class ShapeExpr
object ShapeExpr {
  // logic: ndim = ?
  case class ExactDim(dims: Seq[Value]) extends ShapeExpr
  // logic:
  case class Broadcast(lhs: Value, rhs: Value) extends ShapeExpr
  // logic: canBroadcast(_, v)
  case class BroadcastCompat(v: Value) extends ShapeExpr
}

f(a, b)
// (ExactDim, ExactDim) => check ndims, compare items
// (Broadcast(x, y), Broadcast(z, w)) => fail?
// (_, BroadcastCompat(y)) => canBroadcast(y, a)
// (BroadcastCompat(x), _) => canBroadcast(x, b)
// (BroadcastCompat(x), BroadcastCompat(y)) => check canBroadcast(x, b) || canBroadcast(y, a)

trait Numeral[A] {
}

trait NDArray[A] {
  @refine("_ >= 0")
  val ndim: Int
  @refine("{#1: @Nat, size: _ == this.ndim}")
  val shape: Seq[Int] // size == ndim
  @refine("_ == @core.int.mul(@core.seq.items(this.shape))")
  val size: Int
  val dtype: Numeral[A]

  @refine("rhs: {shape: @nd.shape.canBroadcast(this.shape, _)}, _: {shape: _ == @nd.shape.broadcast(this.shape, rhs.shape)}")
  def +(rhs: NDArray[A]): NDArray[A]

  @refine("this: {ndim: _ <= 2}, rhs: {ndim: _ <= 2}")
  def dot(rhs: NDArray[A]): A

  @refine("@nd.shape.Transpose(this)")
  def transpose: NDArray[A]

  @refine("newshape: {@items: }")
  def reshape(newShape: Int*): NDArray[A]

  def apply(indices: Index*): NDArray[A]
}
object NDArray {
  implicit def scalar2NDArray[A: Numeral](scalar: A): NDArray[A] = ???
  @refine("array: @nd.Shape(1)")
  implicit def NDArray2Scalar[A](array: NDArray[A]): A

  def zeros[A: Numeral](shape: Int*): NDArray[A] = ???
  def zerosLike[A](shape: NDArray[A]): NDArray[A] = ???

  def concatnate[A](axis: Int)(arrays: NDArray[A]*): NDArray[A] = ???
  def vstack[A](arrays: NDArray[A]*): NDArray[A] = concatnate(0)(arrays: _*) = ???
}

object NDArrayTest {
  private[this] val nd = NDArray
  @refine("@nd.Shape(3)")
  val a = NDArray(1, 2, 3)

  @refine("@nd.Shape(3, 2)")
  val b = NDArray(3, 2)(
    1, 2, 3,
    4, 5, 6,
  )

  @refine("@nd.Shape(3, 2)")
  val c = a + b

  @refine("@nd.Shape(2, 1, 3)")
  val d = c.reshape(2, 1, 3)

  @refine("@nd.Shape(2, 3)")
  val e = c.transpose

  @refine("@nd.Shape(2, 3, 4, 5, 6)")
  val f = nd.zeros(2, 3, 1, 5, 1) * nd.zeros(4, 5, 6)

  @refine("b: @nd.BroadcastCompat(a), _: @nd.Broadcast(a, b)")
  def f(a: NDArray, b: NDArray): Unit = {
    a + b
  }
}
