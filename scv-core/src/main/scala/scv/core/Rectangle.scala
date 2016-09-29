package scv.core

sealed abstract class Rectangle[N: Fractional] {

  val xTopLeft: N
  val yTopLeft: N
  val width: N
  val height: N

  val isNormalized: Boolean

  val bounds: Boundaries[N]
  lazy val maxWidth = bounds.maxWidth
  lazy val maxHeight = bounds.maxHeight

  final def copy(
    xTopLeft: N = null.asInstanceOf[N],
    yTopLeft: N = null.asInstanceOf[N],
    width: N = null.asInstanceOf[N],
    height: N = null.asInstanceOf[N],
    bounds: Boundaries[N] = null.asInstanceOf[Boundaries[N]]
  ): Rectangle[N] = Rectangle(
    xTopLeft = if (null != xTopLeft) xTopLeft else this.xTopLeft,
    yTopLeft = if (null != yTopLeft) yTopLeft else this.yTopLeft,
    width = if (null != width) width else this.width,
    height = if (null != height) height else this.height,
    isNormalized = this.isNormalized,
    bounds = if (null != bounds) bounds else this.bounds
  ).get

}

object Rectangle {

  type F = Rectangle[Float]
  type D = Rectangle[Double]

  def apply[N: Fractional](
    xTopLeft: N,
    yTopLeft: N,
    width: N,
    height: N,
    isNormalized: Boolean,
    bounds: Boundaries[N]
  ): Option[Rectangle[N]] =
    if (check(xTopLeft) &&
      check(yTopLeft) &&
      check(width) &&
      check(height) &&
      check(bounds.maxHeight) &&
      check(bounds.maxWidth) &&
      n.lteq(xTopLeft, bounds.maxWidth) &&
      n.lteq(yTopLeft, bounds.maxHeight)) {

      if (!isNormalized ||
        (isNormalized &&
          n.lteq(xTopLeft, n.one) &&
          n.lteq(yTopLeft, n.one) &&
          n.lteq(width, n.one) &&
          n.lteq(height, n.one)))

        Some {
          apply_h(
            x = xTopLeft,
            y = yTopLeft,
            w = width,
            h = height,
            i = isNormalized,
            b = bounds
          )
        }

      else
        None
    } else
      None

  private[core] def apply_h[N: Fractional](
    x: N,
    y: N,
    w: N,
    h: N,
    i: Boolean,
    b: Boundaries[N]
  ): Rectangle[N] =
    new Rectangle[N] {
      override val xTopLeft = x
      override val yTopLeft = y
      override val width = w
      override val height = h
      override val isNormalized = i
      override val bounds = b
    }

  //
  // Helpers
  //

  def top[N: Fractional](r: Rectangle[N]): N =
    r.yTopLeft

  def bottom[N: Fractional](r: Rectangle[N]): N =
    n.plus(r.yTopLeft, r.height)

  def left[N: Fractional](r: Rectangle[N]): N =
    r.xTopLeft

  def right[N: Fractional](r: Rectangle[N]): N =
    n.plus(r.xTopLeft, r.width)

  /** Constructs a Rectangle[Float] from a java.awt.Rectangle instance. */
  def fromJavaRect[N: Fractional](b: Boundaries[N])(r: java.awt.Rectangle): Rectangle[N] =
    Rectangle(
      xTopLeft = n.fromInt(r.x),
      yTopLeft = n.fromInt(r.y),
      width = n.fromInt(r.width),
      height = n.fromInt(r.height),
      isNormalized = false,
      bounds = b
    ).get

  def toJavaRect[N: Fractional](r: Rectangle[N]): java.awt.Rectangle = {
    val (xLeft, yTop, width, height) = rectangleToXYWH(r)
    new java.awt.Rectangle(
      n.toInt(xLeft), // x
      n.toInt(yTop), // y
      n.toInt(yTop), // width
      n.toInt(height) // height
    )
  }

  type XleftYtopWidthHeight[N] = (N, N, N, N)

  /** Extracts the (xLeft,yTop,width,height) information from a Rectangle. */
  def rectangleToXYWH[N: Fractional](r: Rectangle[N]): XleftYtopWidthHeight[N] = {
    val (xLeft, yTop, width, height) =
      if (r.isNormalized) {
        val hPage = r.maxHeight
        val wPage = r.maxWidth
        (
          n.times(r.xTopLeft, wPage),
          n.times(r.yTopLeft, hPage),
          n.times(r.width, wPage),
          n.times(r.height, hPage)
        )
      } else
        (
          r.xTopLeft,
          r.yTopLeft,
          r.width,
          r.height
        )

    (xLeft, yTop, width, height)
  }

  def denormalize[N: Fractional](r: Rectangle[N]): Rectangle[N] =
    if (r.isNormalized) {
      val (xTopLeft, yTopLeft, width, height) = rectangleToXYWH(r)
      apply_h(
        x = xTopLeft,
        y = yTopLeft,
        w = width,
        h = height,
        i = false,
        b = r.bounds
      )

    } else r

  def normalize[N: Fractional](r: Rectangle[N]): Rectangle[N] =
    if (r.isNormalized)
      r

    else {
      val normX = n.div(r.xTopLeft, r.maxWidth)
      val normY = n.div(r.yTopLeft, r.maxHeight)
      val normW = n.div(r.width, r.maxWidth)
      val normH = n.div(r.height, r.maxHeight)
      apply_h(
        x = normX,
        y = normY,
        w = normW,
        h = normH,
        i = true,
        b = r.bounds
      )
    }

  def pretty[N: Fractional](r: Rectangle[N]): String = {
    import r._
    s"[Top Left: (x: $xTopLeft , y: $yTopLeft)  Width: $width , Height: $height]"
  }

  type XY[N] = (N, N)
  def centerXY[N: Fractional](r: Rectangle[N]): XY[N] = {
    val (x, y, w, h) = rectangleToXYWH(r)
    val centerX = n.plus(x, n.div(w, two))
    val centerY = n.plus(y, n.div(h, two))
    (centerX, centerY)
  }

  //
  // private helpers
  //

  @inline private[core] def n[N: Fractional] =
    implicitly[Fractional[N]]

  @inline private[core] def check[N: Fractional](number: N): Boolean =
    n.gteq(number, n.zero)

  @inline private[this] def two[N: Fractional]: N =
    n.plus(n.one, n.one)

}