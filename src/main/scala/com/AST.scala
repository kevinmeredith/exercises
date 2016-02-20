package com

object AST {
	sealed trait Triangle
	case object Equilateral extends Triangle
	case object Isosceles extends Triangle
	case object Scalene extends Triangle

	sealed trait InvalidTriangle
	case object NotAllSidesPositive extends InvalidTriangle
	case object DoesNotEqual180 extends InvalidTriangle

	case class PositiveInt private(x: Int)

	object PositiveInt {
		def build(x: Int): Option[PositiveInt] = if (x > 0) Some(PositiveInt(x)) else None
	}
}