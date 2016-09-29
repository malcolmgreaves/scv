package scv.core

object LocationDslT {

  def apply[T: LocationDsl]: LocationDsl[T] =
    implicitly[LocationDsl[T]]

  object Implicits {

    import Rectangle._

    implicit val RectF: LocationDsl[F] = rectN[Float]

    implicit val RectD: LocationDsl[D] = rectN[Double]

    implicit def rectN[N: Fractional]: LocationDsl[Rectangle[N]] =
      new LocationDsl[Rectangle[N]] {

        override def isLeft(a: Rectangle[N])(b: Rectangle[N]): Boolean =
          n.lteq(right(a), left(b))

        override def isRight(a: Rectangle[N])(b: Rectangle[N]): Boolean =
          n.gteq(left(a), right(b))

        override def isAbove(a: Rectangle[N])(b: Rectangle[N]): Boolean =
          n.lteq(bottom(a), top(b))

        override def isBelow(a: Rectangle[N])(b: Rectangle[N]): Boolean =
          n.gteq(top(a), bottom(b))
      }
  }

}