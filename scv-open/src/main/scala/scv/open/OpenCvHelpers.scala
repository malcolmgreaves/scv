package scv.open

import org.bytedeco.javacpp.helper.opencv_core.CvArr
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_imgproc
import org.bytedeco.javacpp.opencv_imgproc._
import sutils.fp.Types.Err

object OpenCvHelpers {

  def toGrayScale(img: IplImage): IplImage =
    if (img.arrayChannels == 3) {
      val size = cvSize(img.width, img.height)
      val grayImgArr = cvCreateImage(size, img.depth, 1)
      cvCvtColor(img, grayImgArr, CV_BGR2GRAY)
      grayImgArr

    } else img

  def toGrayScale(img: Mat): Mat =
    if (img.channels == 3) {
      val newMat = new Mat(img.arrayHeight, img.arrayWidth, CV_8UC1)
      cvtColor(img, newMat, COLOR_BGR2GRAY)
      newMat

    } else img

  def threshold(
    in: Mat,
    threshold: Double,
    maxValue: Double,
    thresholdType: Int
  ): Mat = {
    val out = in.clone()
    opencv_imgproc.threshold(in, out, threshold, maxValue, thresholdType)
    out
  }

  def cvThreshold(
    in: CvArr,
    threshold: Double,
    maxValue: Double,
    thresholdType: Int
  ): CvArr = {
    val size = cvSize(in.arrayWidth, in.arrayHeight)
    val out = cvCreateImage(size, in.arrayDepth, 1)
    opencv_imgproc.cvThreshold(in, out, threshold, maxValue, thresholdType)
    out
  }

  def adaptiveThreshold(
    in: Mat,
    maxValue: Double,
    adaptiveMethod: Int,
    thresholdType: Int,
    blockSize: Int,
    c: Double
  ): Mat = {
    val out = in.clone()
    opencv_imgproc.adaptiveThreshold(in, out, maxValue, adaptiveMethod, thresholdType, blockSize, c)
    out
  }

  def cvAdaptiveThreshold(
    in: CvArr,
    maxValue: Double,
    adaptiveMethod: Int,
    thresholdType: Int,
    blockSize: Int,
    param1: Double
  ): CvArr = {
    val size = cvSize(in.arrayWidth, in.arrayHeight)
    val out = cvCreateImage(size, in.arrayDepth, 1)
    opencv_imgproc.cvAdaptiveThreshold(in, out, maxValue, adaptiveMethod, thresholdType, blockSize, param1)
    out
  }

  def cloneCvArr(in: CvArr): CvArr = {
    val size = cvSize(in.arrayWidth, in.arrayHeight)
    val out = cvCreateImage(size, in.arrayDepth(), 1)
    org.bytedeco.javacpp.opencv_core.cvCopy(in, out)
    out
  }

  def cloneIplImage(in: IplImage): IplImage = {
    val size = cvSize(in.arrayWidth, in.arrayHeight)
    val out = cvCreateImage(size, in.arrayDepth(), 1)
    org.bytedeco.javacpp.opencv_core.cvCopy(in, out)
    out
  }

  def connectedComponents(
    in: Mat,
    connectivity: Int,
    iType: Int
  ): ConnectComps = {
    val labels = Mat.zeros(in.rows, in.cols, in.`type`).asMat
    opencv_imgproc.connectedComponents(in, labels, connectivity, iType)
    ConnectComps(labels = labels)
  }

  def connectedComponentsWithStats(
    in: Mat,
    connectivity: Int,
    lType: Int
  ): ConnectComps = {

    val labels = Mat.zeros(in.rows, in.cols, in.`type`).asMat
    val stats = Mat.zeros(in.rows, in.cols, in.`type`).asMat
    val centroids = Mat.zeros(in.rows, in.cols, in.`type`).asMat

    opencv_imgproc.connectedComponentsWithStats(in, labels, stats, centroids, connectivity, lType)
    ConnectComps(labels = labels, stats = Some(stats), centroids = Some(centroids))
  }

  def applyBoxes(
    img: Mat,
    boxes: Seq[Rect],
    color: Scalar,
    thickness: Int
  ): Mat = {
    val copy = img.clone
    boxes.foreach { currRect =>
      rectangle(copy, currRect.tl(), currRect.br(), color, thickness, 8, 0)
    }
    copy
  }

  def maskContours(img: Mat, contours: Err[Seq[Rect]]): Mat = {

    val (min, max) = {
      val mins = Array.fill(1)(0.0D)
      val maxes = Array.fill(1)(0.0D)
      minMaxLoc(img, mins, maxes, new Point(), new Point(), null)
      (mins(0), maxes(0))
    }

    val copy = img.clone
    val (origWt, origHt) = (img.arrayWidth, img.arrayHeight)

    contours.foreach { maybeContours =>
      maybeContours.foreach { maybeRoi =>
        val (ht, wt) = (maybeRoi.height, maybeRoi.width)
        rectangle(copy, maybeRoi, new Scalar(max, 0.0, 0.0, 0.0), -1, 8, 0)
      }
    }

    copy
  }

  def bitwiseNot(mat: Mat): Mat = {
    val result = new Mat()
    bitwise_not(mat, result)
    result
  }

  def bitwiseAnd(mat1: Mat, mat2: Mat): Mat = {
    val result = new Mat()
    bitwise_and(mat1, mat2, result)
    result
  }

  def erode(mat: Mat, kernel: Mat): Mat = {
    val result = new Mat()
    opencv_imgproc.erode(mat.clone, result, kernel)
    result
  }

  def erode(
    mat: Mat,
    kernel: Mat,
    anchor: Point,
    iterations: Int,
    borderType: Int,
    borderValue: Scalar
  ): Mat = {
    val result = new Mat()
    opencv_imgproc.erode(mat, result, kernel, anchor, iterations, borderType, borderValue)
    result
  }

  def dilate(mat: Mat, kernel: Mat): Mat = {
    val result = new Mat()
    opencv_imgproc.dilate(mat.clone, result, kernel)
    result
  }

  def dilate(
    mat: Mat,
    kernel: Mat,
    anchor: Point,
    iterations: Int,
    borderType: Int,
    borderValue: Scalar
  ): Mat = {
    val result = new Mat()
    opencv_imgproc.dilate(mat, result, kernel, anchor, iterations, borderType, borderValue)
    result
  }

  def findContours(mask: Mat, mode: Int, method: Int): MatVector = {
    val vec = new MatVector()
    val hierarchy = new Mat()
    val contourPoint = new Point(0, 0)
    opencv_imgproc.findContours(mask, vec, hierarchy, mode, method, contourPoint)
    vec
  }

}