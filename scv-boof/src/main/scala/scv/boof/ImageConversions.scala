package scv.boof

import boofcv.alg.filter.binary.{ GThresholdImageOps, ThresholdImageOps }
import boofcv.gui.binary.VisualizeBinaryData
import boofcv.io.image.ConvertBufferedImage
import boofcv.struct.image.{ GrayF32, GrayU8 }

import scv.core.JavaImage

object ImageConversions {

  //
  // grayscale
  //

  /** Converts a BufferedImage into a gray image, stored in a float matrix. */
  lazy val grayscaleBi: JavaImage => GrayImage =
    x => GrayImage {
      ConvertBufferedImage.convertFrom(
        JavaImage(x),
        null.asInstanceOf[GrayImage.Content]
      )
    }

  //
  // binarization
  //

  /** Binarizes a Java BufferedImage into a byte matrix */
  lazy val binarizeBi: JavaImage => BinaryImage = {
    grayscaleBi andThen GrayImage.apply andThen binarizeGf32(GlobalTC.down)
  }

  /** Converts a binary image (byte matrix) into a Java BufferedImage. */
  lazy val asBi: BinaryImage => JavaImage =
    x => JavaImage {
      VisualizeBinaryData.renderBinary(
        BinaryImage(x),
        false,
        null.asInstanceOf[JavaImage.Content]
      )
    }

  /** Uses the thresholding configuration to binarize a byte matrix. */
  lazy val binarizeGu8: ThresholdC => GrayU8 => BinaryImage = {

    case LocalTC(radius, scale, isThresholdDown) =>
      x => BinaryImage {
        ThresholdImageOps.localSquare(x, null, radius, scale, isThresholdDown, null, null)
      }

    case GlobalTC(isThresholdDown) =>
      x => BinaryImage {
        val threshold = GThresholdImageOps.computeOtsu(x, 0, 255)
        ThresholdImageOps.threshold(x, null, threshold, isThresholdDown)
      }

    case ConstantTC(threshold, isThresholdDown) =>
      x => BinaryImage {
        ThresholdImageOps.threshold(x, null, threshold, isThresholdDown)
      }
  }

  /** Uses the thresholding configuration to binarize a float matrix. */
  lazy val binarizeGf32: ThresholdC => GrayF32 => BinaryImage = {
    case LocalTC(radius, scale, isThresholdDown) =>
      x => BinaryImage {
        ThresholdImageOps.localSquare(x, null, radius, scale, isThresholdDown, null, null)
      }

    case GlobalTC(isThresholdDown) =>
      x => BinaryImage {
        val threshold = GThresholdImageOps.computeOtsu(x, 0, 255)
        ThresholdImageOps.threshold(x, null, threshold, isThresholdDown)
      }

    case ConstantTC(threshold, isThresholdDown) =>
      x => BinaryImage {
        ThresholdImageOps.threshold(x, null, threshold, isThresholdDown)
      }
  }

}