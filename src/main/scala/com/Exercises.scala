package com

import com.AST._

object Exercises {
	
	// 1. Write a function that takes three sides of a triangle and answers if 
	//  it's equilateral, isosceles, or scalene.

	def triangleType(side1: Int, side2: Int, side3: Int): Either[InvalidTriangle, Triangle] = 
		positiveSides(side1, side2, side3) match {
			case None 			 => Left(NotAllSidesPositive)
			case Some(positives) => Right( categorize(positives) )
		}	

	private def positiveSides(x: Int, y: Int, z: Int): Option[(PositiveInt, PositiveInt, PositiveInt)] = 
		for {
			a <- PositiveInt.build(x)
			b <- PositiveInt.build(y)
			c <- PositiveInt.build(z)
		} yield (a,b,c)

	private def categorize(sides: (PositiveInt, PositiveInt, PositiveInt)): Triangle = {
		val (side1, side2, side3) = sides
		List(side1, side2, side3).distinct match {
			case _ :: Nil      => Equilateral
			case _ :: _ :: Nil => Isosceles
			case _ 			   => Scalene
		}
	}

	// 2. For a single-linked (forward only) list write a function that returns 5th 
	//  element from the end of the list. The list can only be walked once 
	// (reverse, length, or size of this list cannot be used).

	// Note - if I were writing this without these given restrictions, I'd write:
	// `xs.reverse.drop(4).take(1).headOption`
	def fifthFromLast[A](xs: List[A]): Option[A] = {
		val result: (Int, Option[A]) = xs.foldRight[(Int, Option[A])]((1, None)) { (elem: A, acc: (Int, Option[A])) => 
			acc match {
				case matched @ (_, Some(_)) => matched
				case (5, _) 			    => (5, Some(elem))
				case (x, _)			   	    => (x+1, None)
			}
		}
		lastHelper(result)
	}

	private def lastHelper[A](result: (Int, Option[A])): Option[A] =
		result._2

	// After walking the list, an O(n) operation, I'd expect that a map look-up, O(1),
	//  would be acceptable. However, I'm not sure if the intention of Exercise 2 was:
	//  walk the list once and that's it. 
	def fifthFromLastAlternative[A](xs: List[A]): Option[A] = {
		val asByIndex: (Int, Map[Int, A]) = xs.foldRight((1, Map.empty[Int, A])) { (elem: A, acc: (Int, Map[Int, A])) =>
			val (index, map) = acc
			(index+1, map + (index -> elem))
		}
		asByIndex._2.get(5)
	}

	// 3. Given two lists, write a function that answers if all elements of one list are in the other. 

	// Assumption: sameElements( List(1,2,3), List(1,1,2,3) ) === false
	// but sameElements( List(1,2,3), List(2,3,1) ) === true
	// In other words, the lists' elements must be exactly the same, excluding order.
	def sameElements[A](xs: List[A], ys: List[A]): Boolean = 
		bag(xs) == bag(ys)

	// https://en.wikipedia.org/wiki/Multiset
	private [com] def bag[A](xs: List[A]): Map[A, Int] = 
		xs.foldRight(Map.empty[A, Int]){ (elem: A, acc: Map[A, Int]) =>
			acc.get(elem) match {
				case None       => acc + (elem -> 1)
				case Some(freq) => acc.updated(elem, 1 + freq)
			}
		}
}