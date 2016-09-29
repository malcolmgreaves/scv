package scv.boof

sealed trait ThresholdC {
  val isThresholdDown: Boolean
}

case class GlobalTC(
  override val isThresholdDown: Boolean = true
) extends ThresholdC

object GlobalTC {
  val down = GlobalTC(isThresholdDown = true)
  val up = GlobalTC(isThresholdDown = false)
}

case class LocalTC(
  radius: Int = 50,
  scale: Float = 0.95f,
  override val isThresholdDown: Boolean = true
) extends ThresholdC

case class ConstantTC(
  threshold: Int,
  override val isThresholdDown: Boolean = true
) extends ThresholdC