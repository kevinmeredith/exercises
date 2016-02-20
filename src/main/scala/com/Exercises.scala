package com

object Exercises {
	

// 2. For a single-linked (forward only) list write a function that returns 5th 
//  element from the end of the list. The list can only be walked once 
// (reverse, length, or size of this list cannot be used).

// assumption: 5th from end means:
// Note that this function is equivalent to: xs.reverse.drop(4).take(1).headOption
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

def fifthFromLastAlternative[A](xs: List[A]): Option[A] = {
	val asByIndex: (Int, Map[Int, A]) = xs.foldRight((1, Map.empty[Int, A])) { (elem: A, acc: (Int, Map[Int, A])) =>
		val (index, map) = acc
		(index+1, map + (index -> elem))
	}
	asByIndex._2.get(5)
}

private def lastHelper[A](result: (Int, Option[A])): Option[A] =
	result._2

}