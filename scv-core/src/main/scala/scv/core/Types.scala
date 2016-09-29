package scv.core

import scalaz.{ Tag, _ }

object Types {

  type Height = Int @@ Height.T
  object Height {
    def apply(x: Int): Height = Tag[Int, T](x)
    def apply(x: Height): Int = Tag.unwrap(x)
    sealed trait T
  }

  type Width = Int @@ Width.T
  object Width {
    def apply(x: Int): Width = Tag[Int, T](x)
    def apply(x: Width): Int = Tag.unwrap(x)
    sealed trait T
  }

  type HasIntWidthHeight = {
    def getWidth: Int
    def getHeight: Int
  }

}