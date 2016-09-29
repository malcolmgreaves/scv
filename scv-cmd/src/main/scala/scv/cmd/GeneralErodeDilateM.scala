package scv.cmd

import boofcv.struct.image.{GrayU8, ImageType, _}
import java.awt.image.BufferedImage
import java.util.concurrent.TimeUnit
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing._

import boofcv.alg.color.ColorRgb
import boofcv.alg.filter.binary.impl.ImplBinaryInnerOps
import boofcv.alg.filter.binary.{Contour, GThresholdImageOps, ThresholdImageOps, _}
import boofcv.core.image.ConvertImage
import boofcv.gui.ListDisplayPanel
import boofcv.gui.binary.VisualizeBinaryData
import boofcv.gui.feature.{ImageLinePanel, VisualizeShapes}
import boofcv.gui.image.ShowImages
import boofcv.io.image.{ConvertBufferedImage, UtilImageIO}
import boofcv.struct.ConnectRule

import georegression.struct.point.Point2D_I32
import georegression.struct.shapes.Rectangle2D_I32
import spire.syntax.cfor._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.language.reflectiveCalls

import scv.boof._
import scv.core._

object GeneralErodeDilateM extends App {

  val startAll = System.currentTimeMillis()

  import scala.collection.JavaConverters._

  import GeneralErodeDilate._
  import BuffAndBinaryImageUtils._

  lazy val c2str: Boolean => Contour => String =
    doLimit => c => {
      val x = s"""(id:${c.id}) Exterior(${c.external.size}): ${c.external.asScala.mkString(",")}"""
      if (doLimit)
        x.substring(0, math.min(x.length, 30))
      else
        x
    }

  val msg: Long => String =
    diff => s"Elapsed time ${Duration(diff, TimeUnit.MILLISECONDS).toSeconds} seconds ($diff ms)"

  val calculate = (image: BinaryImage) => {

    val (origMinV, origMaxV) = {
      val values = image.data
      (values.min, values.max)
    }

    val (gMinV, gMaxV) = {
      val get = getFromU8(image)
      val values = for {
        x <- 0 until image.width
        y <- 0 until image.height
      } yield get(x, y)
      (values.min, values.max)
    }

    (
      (origMinV, origMaxV),
      (gMinV, gMaxV)
      )
  }

  import RunConcurrent.ImplicitDefaultVals._

  // MUTATION WARNING !!
  private[this] val listPanel = new ListDisplayPanel

  // MUTATION WARNING !!
  def addImageToPanel(bi: BufferedImage, message: String): Unit = {
    val gui = new ImageLinePanel
    gui.setBackground(bi)
    gui.setPreferredSize(new Dimension(bi.getWidth, bi.getHeight))
    listPanel.addItem(gui, message)
  }

  //  val (sizeL, sizeS) = (500, 10)
  //  val (horizL, vertL, horizS, vertS) = (
  //    RectStructElem.horizontal(sizeL), RectStructElem.vertical(sizeL),
  //    RectStructElem.horizontal(sizeS), RectStructElem.vertical(sizeS)
  //  )
  //  val (dilateHorizL, dilateVertL, dilateHorizS, dilateVertS) = (
  //    dilateR(horizL), dilateR(vertL), dilateR(horizS), dilateR(vertS)
  //  )
  //  val (erodeHorizL, erodeVertL, erodeHorizS, erodeVertS) = (
  //    erodeR(horizL), erodeR(vertL), erodeR(horizS), erodeR(vertS)
  //  )

  import ErodeDilateVertHorizFns.default.{ horizontalResponse, verticalResponse }

  //  val (dilateH, dilateV, erodeH, erodeV) = {
  //    val h = RectStructElem.horizontal(150)
  //    val v = RectStructElem.vertical(55)
  //    (dilateR(h), dilateR(v), erodeR(h), erodeR(v))
  //  }

  ////////////////////////////////

  val startLoad = System.currentTimeMillis()

  val in = args.head
  println(s"Operating on: $in")

  val asBufImgOriginal = UtilImageIO.loadImage(in)

  val original = ConvertBufferedImage.convertFrom(
    asBufImgOriginal,
    true,
    ImageType.pl(4, classOf[BaseImage])
  )

  val grayscale: BaseImage = {
    val g = new BaseImage(original.width, original.height)
    ColorRgb.rgbToGray_Weighted_U8(original, g)
    g
  }

  //  val conv = {
  //    println("GLOBAL thresholding")
  //    convertToBW(GlobalT(isThresholdDown = true))
  //  }

  val binary: BinaryImage = {

    val conv = Binarize.convertG8ToBW(ConstantT.forDocumentInGrayU8)

    val r = conv(original.getBand(0))
    val g = conv(original.getBand(1))
    val b = conv(original.getBand(2))

    BinaryImageOps.logicOr(BinaryImageOps.logicOr(r, g, null), b, null)
  }

  //  println {
  //
  //    val ((o_aMinV, o_aMaxV), (o_gMinV, o_gMaxV)) = calculate(original)
  //    val ((b_aMinV, b_aMaxV), (b_gMinV, b_gMaxV)) = calculate(binary)
  //
  //    s"""Pixel Value Inspection for *ORIGINAL* image ::
  //         |[From Array[Byte]]  Minimum pixel value: $o_aMinV and maximum pixel value: $o_aMaxV
  //         |[From getFromU8]    Minimum pixel value: $o_gMinV and maximum pixel value: $o_gMaxV
  //         |
  //         |Pixel Value Inspection for *BINARY* image ::
  //         |[From Array[Byte]]  Minimum pixel value: $b_aMinV and maximum pixel value: $b_aMaxV
  //         |[From getFromU8]    Minimum pixel value: $b_gMinV and maximum pixel value: $b_gMaxV
  //        """.stripMargin
  //  }

  //
  println(s"[Load and Binarize Image]  ${msg(System.currentTimeMillis() - startLoad)}")
  //

  //
  // [begin] IMAGE MORPHOLOGY
  //
  val startCustom = System.currentTimeMillis()

  //  val (lgD_H, lgD_V, smD_H, smD_V, lgED_H, lgED_V) =
  //    RunConcurrent(
  //      binary,
  //      dilateHorizL,
  //      dilateVertL,
  //      dilateHorizS,
  //      dilateVertS,
  //      erodeHorizL andThen dilateHorizL,
  //      erodeVertL andThen dilateVertL
  //    )

  //    val (lgED_H, lgED_V) =
  //      RunConcurrent(
  //        binary,
  //        erodeHorizL andThen dilateHorizL,
  //        erodeVertL andThen dilateVertL
  //      )

  val lgED_H = horizontalResponse(binary)
  val lgED_V = verticalResponse(binary)
  val lgED_Or = {
    BinaryImageOps.logicOr(
      BinaryImageOps.invert(lgED_H, null),
      BinaryImageOps.invert(lgED_V, null),
      null
    )
  }

  //
  println(s"[Perform Custom Ops]       ${msg(System.currentTimeMillis() - startCustom)}")
  //

  val startContours = System.currentTimeMillis()

  val contours =
    BinaryImageOps.contour(lgED_Or, ConnectRule.EIGHT, null)
      .asScala
      .toIndexedSeq

  println(s"[Find Contours]           ${msg(System.currentTimeMillis() - startContours)}")

  println(
    s"""Found (${contours.size}) contours:
        |${"\t"}${contours.map { c2str(true) }.mkString("\n\t")}
     """.stripMargin
  )

  val startDisplay = System.currentTimeMillis()

  addImageToPanel(VisualizeBinaryData.renderBinary(lgED_Or, false, null), "Final Combined Result")

  addImageToPanel(VisualizeBinaryData.renderBinary(lgED_H, true, null), "erode then dilate horizontal large")
  addImageToPanel(VisualizeBinaryData.renderBinary(lgED_V, true, null), "erode then dilate vertical large")

  //  addImageToPanel(VisualizeBinaryData.renderBinary(lgD_H, false, null), "Large dilate HORIZONTAL")
  //  addImageToPanel(VisualizeBinaryData.renderBinary(lgD_V, false, null), "Large dilate VERTICAL")
  //
  //  addImageToPanel(VisualizeBinaryData.renderBinary(smD_H, false, null), "Small dilate HORIZONTAL")
  //  addImageToPanel(VisualizeBinaryData.renderBinary(smD_V, false, null), "Small dilate VERTICAL")

  //
  // [end] IMAGE MORPHOLOGY
  //

  addImageToPanel(VisualizeBinaryData.renderBinary(binary, false, null), "Original Binarized")
  addImageToPanel(ConvertBufferedImage.convertTo(grayscale, null), "Original Image as Grayscale")
  addImageToPanel(asBufImgOriginal, "Original Image as RGB")

  addImageToPanel(
    {

      val copy = Helpers.copyOfBuffImg(asBufImgOriginal)

      val g: Graphics2D = copy.createGraphics()
      g.setStroke(new BasicStroke(10))

      g.setColor(Color.RED)
      contours.foreach { c =>
        VisualizeShapes.drawPolygon(c.external, false, g)
      }

      g.dispose()

      copy
    },
    "Located Contours, Exterior (RED)"
  )

  addImageToPanel(
    {

      val copy = Helpers.copyOfBuffImg(asBufImgOriginal)

      val g: Graphics2D = copy.createGraphics()
      g.setStroke(new BasicStroke(10))

      g.setColor(Color.BLUE)
      contours.foreach { c =>
        c.internal.asScala.foreach { interiors =>
          VisualizeShapes.drawPolygon(interiors, false, g)
        }
      }

      g.dispose()

      copy
    },
    "Located Contours, Interior (BLUE)"
  )

  addImageToPanel(
    {

      val copy = Helpers.copyOfBuffImg(asBufImgOriginal)

      val g: Graphics2D = copy.createGraphics()
      g.setStroke(new BasicStroke(10))

      g.setColor(Color.YELLOW)
      contours.foreach { c =>
        VisualizeShapes.drawPolygon(c.external, true, g)
      }

      g.dispose()

      copy
    },
    "Located Contours, Exterior w/ Fill Draw (YELLOW)"
  )

  addImageToPanel(
    {

      val copy = Helpers.copyOfBuffImg(asBufImgOriginal)

      val g: Graphics2D = copy.createGraphics()
      g.setStroke(new BasicStroke(10))

      g.setColor(Color.ORANGE)
      contours.foreach { c =>
        c.internal.asScala.foreach { interiors =>
          VisualizeShapes.drawPolygon(interiors, true, g)
        }
      }

      g.dispose()

      copy
    },
    "Located Contours, Interior w/ Fill Draw (ORANGE)"
  )

  ShowImages.showWindow(listPanel, "GenerateErodeDilateM", true)
  //
  println(s"[Display Images]           ${msg(System.currentTimeMillis() - startDisplay)}")
  //

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // ///
  println(s"[Entire Program]           ${msg(System.currentTimeMillis() - startAll)}") //
  // // // // // // // // // // // // // // // // // // // // // // // // // // // // ///
}

class ImageComponent(val img: BufferedImage) extends JComponent {

  setZoom_mut(1.0)

  @Override
  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    val dim = getPreferredSize
    val _ = g.drawImage(img, 0, 0, dim.width, dim.height, this)
  }

  def setZoom_mut(zoom: Double) = {
    val w = (zoom * img.getWidth()).toInt
    val h = (zoom * img.getHeight()).toInt
    setPreferredSize(new Dimension(w, h))
    revalidate()
    repaint()
  }
}