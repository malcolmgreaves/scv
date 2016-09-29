package scv.boof

import boofcv.struct.image.GrayU8
import spire.syntax.cfor._

import scalaz.{ @@, Tag }

object GeneralErodeDilate {

  /////////////////////////////////////////////////////////////////////////////
  //////                                                                 //////
  //////                 T y p e   D e f i n i t i o n s                 //////
  //////                                                                 //////
  /////////////////////////////////////////////////////////////////////////////

  final val active: Byte = 1
  final val inactive: Byte = 0

  type Index = Int
  type Stride = Int

  case class BorderLogic(fill: Byte)
  object BorderLogic {
    lazy val ones = BorderLogic(1)
    lazy val zeros = BorderLogic(0)
  }

  type RawValueView = RawValueView.Content @@ RawValueView.Phantom
  object RawValueView {
    def apply(x: Content): RawValueView = Tag[Content, Phantom](x)
    def apply(x: RawValueView): Content = Tag.unwrap(x)
    sealed trait Phantom
    type Content = Array[Boolean]
  }

  abstract class MutableState

  trait MutActivationAccum extends MutableState {

    final def observe(value: Boolean): Unit =
      observeMany_h(value, 1)

    final def observe(value: Boolean, positiveNumObservations: Int): Unit =
      if (positiveNumObservations > 0)
        observeMany_h(value, positiveNumObservations)
      else
        ()

    protected def observeMany_h(value: Boolean, times: Int): Unit

    def clear(): Unit

    def result: Boolean
  }

  object MutActivationAccum {

    /** Evaluates to true iff at least one input value is true. */
    lazy val activeAtLeastOne: Constructor[MutActivationAccum] =
      () => new MutActivationAccum {

        private[this] var countActive = 0

        override def observeMany_h(value: Boolean, times: Int): Unit =
          if (value)
            countActive += times
          else
            ()

        override def result: Boolean =
          countActive > 0

        override def clear(): Unit =
          countActive = 0
      }

    /** Evaluates to true iff at least one input value is false. */
    lazy val nonActiveAtLeastOne: Constructor[MutActivationAccum] =
      () => new MutActivationAccum {

        private[this] var countInactive = 0

        protected override def observeMany_h(
          value: Boolean,
          times: Int
        ): Unit =
          if (value)
            ()
          else
            countInactive += times

        override def clear(): Unit =
          countInactive = 0

        override def result: Boolean =
          countInactive > 0
      }

    /** Evaluates to true iff a majority of input values are true. */
    lazy val majorityActive: Constructor[MutActivationAccum] =
      () => new MutActivationAccum {

        private[this] var (count, countActive) = (0, 0)

        protected override def observeMany_h(
          value: Boolean,
          times: Int
        ): Unit = {
          count += times
          if (value)
            countActive += times
          else
            ()
        }

        override def result: Boolean =
          countActive > (count / 2)

        override def clear(): Unit = {
          countActive = 0
          count = 0
        }
      }

    /** Evaluates to true iff all input values are true. */
    lazy val allActive: Constructor[MutActivationAccum] =
      () => new MutActivationAccum {

        private[this] var (count, countActive) = (0, 0)

        protected override def observeMany_h(
          value: Boolean,
          times: Int
        ): Unit = {
          count += times
          if (value)
            countActive += times
          else
            ()
        }

        override def result: Boolean =
          countActive == count

        override def clear(): Unit = {
          countActive = 0
          count = 0
        }
      }

    /** Evaluates to true iff all input values are false. */
    lazy val noneActive: Constructor[MutActivationAccum] =
      () => new MutActivationAccum {

        private[this] var (count, countInactive) = (0, 0)

        protected override def observeMany_h(
          value: Boolean,
          times: Int
        ): Unit = {
          count += times
          if (value)
            ()
          else
            countInactive += times
        }

        override def result: Boolean =
          countInactive == count

        override def clear(): Unit = {
          countInactive = 0
          count = 0
        }
      }

  }

  type Constructor[T] = () => T

  type ImageTransform = BinaryImage => BinaryImage

  /////////////////////////////////////////////////////////////////////////////
  //////                                                                 //////
  //////                I m a g e    M o r p h o l o g y                 //////
  //////                                                                 //////
  /////////////////////////////////////////////////////////////////////////////

  type BinaryMorphlogy = BorderLogic => Constructor[MutActivationAccum] => RectStructElem => ImageTransform
  lazy val generalErosionOrDilation: BinaryMorphlogy =
    border => {
      val fillBoolRes = border.fill != 0

      mkMutActivationAccumulator => rse => {

        // 3rd argument is "size"
        val (widthMod, heightMod, _) = rse match {

          case HorizontalRSE(halfWidth) =>
            (halfWidth, 0, halfWidth * 2 + 1)

          case VerticalRSE(halfHeight) =>
            (0, halfHeight, halfHeight * 2 + 1)

          case SquareRSE(halfSideLen) =>
            val sideLen = halfSideLen * 2
            (halfSideLen, halfSideLen, sideLen * sideLen + 2)

          case GeneralRSE(halfWidth, halfHeight) =>
            val (w, h) = (halfWidth * 2, halfHeight * 2)
            (halfWidth, halfHeight, w * h + 2)
        }

        s => BinaryImage {

          val source = BinaryImage(s)

          val sourceData = source.data
          val maxIndex = sourceData.length
          val (width, height) = (source.getWidth, source.getHeight)

          // BEWARE! BEWARE! the following are mutable / are mutated !!!!!!!!!!
          val maa = mkMutActivationAccumulator()
          val mutImg = new GrayU8(width, height)

          // [begin] MUTATION WARNING
          cfor(0)(_ < width, _ + 1) { x =>
            cfor(0)(_ < height, _ + 1) { y =>

              var index = 0
              var valueAtSource: Byte = 0
              val minX = x - widthMod
              val maxX = x + widthMod
              val minY = y - heightMod
              val maxY = y + heightMod

              // look at the neighboring pixels around (x,y)
              cfor(minY)(_ <= maxY, _ + 1) { yNeighbor =>
                cfor(minX)(_ <= maxX, _ + 1) { xNeighbor =>

                  index = source.getIndex(xNeighbor, yNeighbor)

                  valueAtSource =
                    if (index < 0 || index >= maxIndex)
                      border.fill
                    else
                      sourceData(index)

                  maa.observe(valueAtSource != 0)
                }
              }

              mutImg.set(
                x,
                y,
                if (maa.result) active else inactive
              )
              // DO NOT FORGET TO clear THE ACCUMULATOR !!!!!!!
              maa.clear()

            }
          }
          // [end] MUTATION WARNING

          mutImg
        }
      }
    }

  lazy val erodeR: RectStructElem => ImageTransform =
    generalErosionOrDilation(
      BorderLogic.zeros
    )(
      MutActivationAccum.nonActiveAtLeastOne
    )

  lazy val dilateR: RectStructElem => BinaryImage => BinaryImage =
    generalErosionOrDilation(
      BorderLogic.ones
    )(
      MutActivationAccum.allActive
    )

}