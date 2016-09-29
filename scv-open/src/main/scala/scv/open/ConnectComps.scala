package scv.open

import org.bytedeco.javacpp.opencv_core._

case class ConnectComps(
  labels: Mat,
  stats: Option[Mat] = None,
  centroids: Option[Mat] = None
)