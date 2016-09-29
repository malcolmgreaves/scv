package scv.boof

import boofcv.alg.filter.binary.BinaryImageOps
import GeneralErodeDilate._

sealed trait ErodeDilateVertHorizFns {
  val dilateH: ImageTransform
  val dilateV: ImageTransform
  val erodeH: ImageTransform
  val erodeV: ImageTransform
  val horizontal: RectStructElem
  val vertical: RectStructElem
  final lazy val horizontalResponse: ImageTransform = erodeH andThen dilateH
  final lazy val verticalResponse: ImageTransform = erodeV andThen dilateV
  final lazy val entireResponse: ImageTransform =
    binary => BinaryImage {
      val lgED_H = horizontalResponse(binary)
      val lgED_V = verticalResponse(binary)
      val lgED_Or = {
        BinaryImageOps.logicOr(
          BinaryImageOps.invert(BinaryImage(lgED_H), null),
          BinaryImageOps.invert(BinaryImage(lgED_V), null),
          null
        )
      }
      lgED_Or
    }

  override lazy val toString =
    s"""{
       |  Horizontal Kernel Size: ${horizontal.halfWidth * 2}
       |  Vertical Kernel Size:   ${vertical.halfHeight * 2}
       |}""".stripMargin
}

object ErodeDilateVertHorizFns {

  /**
   * Constructor.
   * @param hsz horizontal kernel size, positive
   * @param vsz vertical kernel size, positive
   */
  def apply(hsz: Int, vsz: Int): ErodeDilateVertHorizFns = {
    assert(hsz > 0)
    assert(vsz > 0)
    new ErodeDilateVertHorizFns {
      override val vertical = RectStructElem.vertical(vsz)
      override val horizontal = RectStructElem.horizontal(hsz)
      override val erodeH = erodeR(horizontal)
      override val dilateH = dilateR(horizontal)
      override val erodeV = erodeR(vertical)
      override val dilateV = dilateR(vertical)
    }
  }
}