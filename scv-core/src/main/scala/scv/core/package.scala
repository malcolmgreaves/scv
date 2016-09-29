package scv

import java.awt.image.BufferedImage

import scalaz.{ Tag, @@ }

package object core {

  type JavaImage = JavaImage.Content @@ JavaImage.Phantom
  object JavaImage {
    def apply(x: Content): JavaImage = Tag[Content, Phantom](x)
    def apply(x: JavaImage): Content = Tag.unwrap[Content, Phantom](x)
    sealed trait Phantom
    type Content = BufferedImage
  }

}
