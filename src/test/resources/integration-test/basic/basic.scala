import com.todesking.prety.refine

object Basic {
  @refine("x: _ > 0")
  def posi(x: Int) = x

  @refine("_: _ > 0")
  def getPosiValue() = 10
  @refine("_: _ < 0")
  def getNegaValue() = -99

  @refine("_: _ > 0")
  def getPosiValue0 = 10
  @refine("_: _ < 0")
  def getNegaValue0 = -10

  @refine("_: _ > 0")
  val posiValue = 10
  @refine("_: _ < 0")
  val negaValue = -1

  @refine("_: _ > 0")
  private[this] val posiValue0 = 10
  @refine("_: _ < 0")
  private[this] val negaValue0 = -1

  def bar(): Unit = {
    val p = 10
    val n = -1

    posi(1)
    posi(p)
    posi(posiValue)
    posi(posiValue0)
    posi(getPosiValue)
    posi(getPosiValue())
    posi(getPosiValue0)

    posi(0)
    //   ^
    posi(n)
    //   ^
    posi(negaValue)
    //   ^
    posi(negaValue0)
    //   ^
    posi(getNegaValue)
    //   ^
    posi(getNegaValue())
    //   ^
    posi(getNegaValue0)
    //   ^
  }
}
