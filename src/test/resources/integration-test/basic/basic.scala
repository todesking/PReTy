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

  // TODO: private[this] val posiValueInferred
  // TODO: val posiValueNotInferred

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
    //   ^{_ == 0} *<= {_ > 0}
    posi(n)
    //   ^{_ == -1} *<= {_ > 0}
    posi(negaValue)
    //   ^{_ < 0} *<= {_ > 0}
    posi(negaValue0)
    //   ^{_ < 0} *<= {_ > 0}
    posi(getNegaValue)
    //   ^{_ < 0} *<= {_ > 0}
    posi(getNegaValue())
    //   ^{_ < 0} *<= {_ > 0}
    posi(getNegaValue0)
    //   ^{_ < 0} *<= {_ > 0}
  }
}
