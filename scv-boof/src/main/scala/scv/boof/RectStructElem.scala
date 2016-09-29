package scv.boof

sealed trait RectStructElem {
  val halfHeight: Int
  val halfWidth: Int
}
case class HorizontalRSE(override val halfWidth: Int) extends RectStructElem {
  override final val halfHeight = 1
}
case class VerticalRSE(override val halfHeight: Int) extends RectStructElem {
  override final val halfWidth = 1
}
case class SquareRSE(override val halfWidth: Int) extends RectStructElem {
  override final lazy val halfHeight = halfWidth
}
case class GeneralRSE(
  override val halfWidth: Int,
  override val halfHeight: Int
) extends RectStructElem

object RectStructElem {

  /**
   * Constructs a rectangular structuring element using the input width and
   * height. Both arguments must be positive.
   */
  def apply(width: Int, height: Int): RectStructElem =
    if (width == height)
      square(width)

    else if (width == 1)
      vertical(height)

    else if (height == 1)
      horizontal(width)

    else
      GeneralRSE(
        halfWidth = bothDivEnsureGeq1(width),
        halfHeight = bothDivEnsureGeq1(height)
      )

  //
  // specific shape constructors
  //

  /** A structuring element with height=1 and width equal to the parameter. */
  lazy val horizontal: Int => HorizontalRSE =
    width => HorizontalRSE(bothDivEnsureGeq1(width))

  /** A structuring element with width=1 and height equal to the parameter. */
  lazy val vertical: Int => VerticalRSE =
    height => VerticalRSE(bothDivEnsureGeq1(height))

  /** A structuring element with height and width equal to one another. */
  lazy val square: Int => SquareRSE =
    sideLength => SquareRSE(bothDivEnsureGeq1(sideLength))

  //
  // helpers
  //

  private[boof] lazy val ensureGeqOne: Int => Int =
    x => if (x <= 1) 1 else x

  private[boof] lazy val div2SafeForOne: Int => Int =
    x => if (x == 1) 1 else x / 2

  private[boof] lazy val bothDivEnsureGeq1 =
    ensureGeqOne andThen div2SafeForOne
}