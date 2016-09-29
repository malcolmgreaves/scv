package scv.core

import simulacrum.typeclass

@typeclass trait LocationDsl[T] {

  def isLeft(a: T)(b: T): Boolean

  def isRight(a: T)(b: T): Boolean

  def isAbove(a: T)(b: T): Boolean

  def isBelow(a: T)(b: T): Boolean

  final def intersect(a: T)(b: T): Boolean =
    !isLeft(a)(b) && !isRight(a)(b) && !isAbove(a)(b) && !isBelow(a)(b)
}