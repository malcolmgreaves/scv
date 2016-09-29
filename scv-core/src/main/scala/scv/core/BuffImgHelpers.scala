package scv.core

import java.awt.color.ColorSpace
import java.awt.image._
import java.io.ByteArrayInputStream
import javax.imageio.ImageIO

import scv.core.Types.{ HasIntWidthHeight, Height, Width }
import sutils.fp.Types.Size

import scala.language.reflectiveCalls
import scala.reflect.ClassTag

object BuffImgHelpers {

  type ProcBi = BufferedImage => BufferedImage

  /** Creates a BufferedImage after copying the input bytes. */
  lazy val asBufferedImage: Seq[Byte] => BufferedImage =
    data => asBufferedImageShare(data.toArray)

  /** Creates a BufferedImage w/o copying the input byte array. */
  lazy val asBufferedImageShare: Array[Byte] => BufferedImage =
    data => ImageIO.read(new ByteArrayInputStream(data))

  lazy val rasterToBuffImg: ColorModel => Raster => BufferedImage =
    cm => ras =>
      new BufferedImage(
        cm,
        {
          val wRas = ras.createCompatibleWritableRaster()
          wRas.setDataElements(0, 0, ras)
          wRas
        },
        cm.isAlphaPremultiplied,
        null.asInstanceOf[java.util.Hashtable[Any, Any]]
      )

  /** Evaluates to the reference of the Raster's internal bytes. */
  def pointerToBytesOf(ras: Raster): Array[Byte] =
    if (ras == null)
      Array.empty
    else
      ras.getDataBuffer.asInstanceOf[DataBufferByte].getData

  def pointerToBytesOf(bi: BufferedImage): Array[Byte] =
    if (bi == null)
      Array.empty
    else
      pointerToBytesOf(bi.getRaster)

  def asValueArray[N: Numeric: ClassTag](bi: BufferedImage): Array[N] = {
    val asN: Int => N = implicitly[Numeric[N]].fromInt _

    val (width, height) = (bi.getWidth, bi.getHeight)

    val pixelLen = {
      import BufferedImage._
      bi.getType match {
        case TYPE_3BYTE_BGR => 3
        case TYPE_4BYTE_ABGR => 4
        case TYPE_BYTE_GRAY | TYPE_BYTE_BINARY | TYPE_INT_RGB | TYPE_INT_ARGB => 1
        case unk => throw new IllegalStateException(
          s"Cannot deal with unknown BufferedImage byte layout (image type): $unk . Expecting one of BufferedImage.TYPE_{3BYTE_BGR, 4BYTE_ABGR, BYTE_GRAY, BYTE_BINARY, INT_RGB, INT_ARGB}."
        )
      }
    }

    val asByteArray = pointerToBytesOf(bi.getRaster)
    val nPixels = width * height
    val res = new Array[N](nPixels)

    // [begin] mutation warning
    var resIndex = 0
    var pixelIndex = 0
    var tmp: Int = 0

    while (pixelIndex < nPixels) {

      res(resIndex) = asN(
        pixelLen match {
          case 1 =>
            asByteArray(pixelIndex)

          case 3 =>
            tmp = 0

            tmp += -16777216 // 255 alpha
            tmp += asByteArray(pixelIndex).toInt & 0xFF // blue
            tmp += (asByteArray(pixelIndex + 1).toInt & 0xFF) << 8 // green
            tmp += (asByteArray(pixelIndex + 2).toInt & 0xFF) << 16 // red

            tmp

          case 4 =>
            tmp = 0

            tmp += (asByteArray(pixelIndex).toInt & 0xFF) << 24 // alpha
            tmp += (asByteArray(pixelIndex).toInt + 1) & 0xFF // blue
            tmp += (asByteArray(pixelIndex + 2).toInt & 0xFF) << 8 // green
            tmp += (asByteArray(pixelIndex + 3).toInt & 0xFF) << 16 // red

            tmp
        }
      )

      // [begin] increment pointers
      resIndex += 1
      pixelIndex += pixelLen
      // [end] increment pointers
    }
    // [end] mutation warning

    res
  }

  lazy val calcNewDimForPixLimit: Size => HasIntWidthHeight => (Width, Height) =
    nPixLimit => originalImage => {

      val (ratioOrigH2W, ratioOrigW2H) = (
        originalImage.getHeight.toDouble / originalImage.getWidth.toDouble,
        originalImage.getWidth.toDouble / originalImage.getHeight.toDouble
      )

      (
        Width(math.floor(math.sqrt(Size(nPixLimit) * ratioOrigW2H)).toInt),
        Height(math.floor(math.sqrt(Size(nPixLimit) * ratioOrigH2W)).toInt)
      )
    }

  lazy val toGray: ProcBi =
    anyKindOfImage => {

      val toBeGray = new BufferedImage(
        anyKindOfImage.getWidth,
        anyKindOfImage.getHeight,
        BufferedImage.TYPE_BYTE_GRAY
      )

      val g = toBeGray.getGraphics
      g.drawImage(anyKindOfImage, 0, 0, null.asInstanceOf[ImageObserver])
      g.dispose()

      toBeGray
    }

  lazy val deepCopy: ProcBi =
    bi => rasterToBuffImg(bi.getColorModel)(bi.copyData(null))

  lazy val convertToRgb: ProcBi = {
    val rgbColorSpace = ColorSpace.getInstance(ColorSpace.CS_LINEAR_RGB)
    val colorType = BufferedImage.TYPE_3BYTE_BGR

    origImg => {
      val rgbImg = new BufferedImage(origImg.getWidth, origImg.getHeight, colorType)
      val op = new ColorConvertOp(rgbColorSpace, null)
      op.filter(origImg, rgbImg)
      rgbImg
    }
  }

  lazy val convertToGrayscale: ProcBi = {
    val grayColorSpace = ColorSpace.getInstance(ColorSpace.CS_GRAY)
    val grayType = BufferedImage.TYPE_BYTE_GRAY

    origImg =>
      if (origImg.getType != grayType) {
        val grayImg = new BufferedImage(origImg.getWidth, origImg.getHeight, grayType)
        val op = new ColorConvertOp(grayColorSpace, null)
        op.filter(origImg, grayImg)
        grayImg

      } else origImg
  }

}