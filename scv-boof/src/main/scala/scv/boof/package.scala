package scv

import boofcv.struct.image.{ GrayF32, GrayU8 }

import scalaz.{ @@, Tag }

package object boof {

  type BinaryImage = BinaryImage.Content @@ BinaryImage.Phantom
  object BinaryImage {
    def apply(x: Content): BinaryImage = Tag[Content, Phantom](x)
    def apply(x: BinaryImage): Content = Tag.unwrap[Content, Phantom](x)
    sealed trait Phantom
    type Content = GrayU8
  }

  type GrayImage = GrayImage.Content @@ GrayImage.Phantom
  object GrayImage {
    def apply(x: Content): GrayImage = Tag[Content, Phantom](x)
    def apply(x: GrayImage): Content = Tag.unwrap[Content, Phantom](x)
    sealed trait Phantom
    type Content = GrayF32
  }

  type BlurRadius = BlurRadius.Content @@ BlurRadius.Phantom
  object BlurRadius {
    def apply(x: Content): BlurRadius = Tag[Content, Phantom](x)
    def apply(x: BlurRadius): Content = Tag.unwrap[Content, Phantom](x)
    sealed trait Phantom
    type Content = Int
  }

}