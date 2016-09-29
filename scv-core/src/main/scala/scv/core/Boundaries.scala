package scv.core

sealed abstract class Boundaries[N: Fractional] {

  val maxWidth: N
  val maxHeight: N

  final def copy(
    maxWidth: N = null.asInstanceOf[N],
    maxHeight: N = null.asInstanceOf[N]
  ): Boundaries[N] =
    Boundaries(
      maxWidth = if (null != maxWidth) maxWidth else this.maxWidth,
      maxHeight = if (null != maxHeight) maxHeight else this.maxHeight
    ).get
}

object Boundaries {

  type F = Boundaries[Float]
  type D = Boundaries[Double]

  import Rectangle._

  def apply[N: Fractional](
    maxWidth: N,
    maxHeight: N
  ): Option[Boundaries[N]] =
    if (check(maxHeight) && check(maxWidth))
      Some {
        apply_h(
          w = maxWidth,
          h = maxHeight
        )
      }
    else
      None

  private[core] def apply_h[N: Fractional](
    w: N,
    h: N
  ): Boundaries[N] =
    new Boundaries[N] {
      override val maxWidth = w
      override val maxHeight = h
    }

}