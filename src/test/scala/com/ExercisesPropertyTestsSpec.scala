package com

import com.Exercises._
import com.AST._

import org.scalatest._
import org.scalatest.prop.PropertyChecks

import org.scalacheck.{Gen, Properties, Arbitrary}
import Gen._
import Arbitrary._
import org.scalacheck.Prop.forAll
import scala.math.Numeric.IntIsIntegral

object ExercisesPropertyTestsSpec {

	private val rand = new java.util.Random

	def randomSizedListMaxSizeN[A](gen: Gen[A], n: Int): Gen[List[A]] = { 
		Gen.listOf(gen).map(_.take(rand.nextInt(n+1))) // Random#nextInt's argument is exclusive, i.e. not inclusive
	}

	def randomListSizeMinSizeN[A](gen: Gen[A], minSize: Int): Gen[List[A]] = 
		for {
			randomA    <- gen
			randomList <- Gen.listOf(gen)
		} yield List.fill(minSize)(randomA) ++ randomList

	def atLeastOneNegativeSide: Gen[(Int, Int, Int)] = for {
		negOrPos <- Gen.oneOf(posNum, negNum)
		neg 	 <- negNum
	} yield (negOrPos, neg, negOrPos)

	def atLeastOneZeroSide: Gen[(Int, Int, Int)] = 
		posNum.map(pos => (pos, pos, 0))

	def positiveSidesSame: Gen[(Int, Int, Int)] = 	
		posNum.map(n => (n, n, n))

	def positive2SidesOnlySame: Gen[(Int, Int, Int)] = for {
		positive 		 <- posNum
		distinctPositive <- posNum suchThat (_ != positive)
	} yield (positive, distinctPositive, positive)

	def positive3UniqueSides: Gen[(Int, Int, Int)] = for {
		positive 		  <- posNum
		distinctPositive  <- posNum suchThat (_ != positive)
		distinctPositive2 <- posNum suchThat (x => x != positive && x != distinctPositive)
	} yield (positive, distinctPositive, distinctPositive2)

}

class ExercisesPropertyTestsSpec extends FunSuite with PropertyChecks {
	
	import ExercisesPropertyTestsSpec._

	test("Categorizing Triangle with 1 or more negative values should fail") {
		forAll( atLeastOneNegativeSide ) { triangleSides => 
			val (side1, side2, side3) = triangleSides
			assert( triangleType(side1, side2, side3) === Left(NotAllSidesPositive) )
		}
	}

	test("Categorizing Triangle with 1 `0` values should fail") {
		forAll( atLeastOneZeroSide ) { triangleSides => 
			val (side1, side2, side3) = triangleSides
			assert( triangleType(side1, side2, side3) === Left(NotAllSidesPositive) )
		}
	}

	test("Categorizing Triangle having 3 positive sides with all equal should return Equilateral") {
		forAll( positiveSidesSame ) { triangleSides => 
			val (side1, side2, side3) = triangleSides
			assert( triangleType(side1, side2, side3) === Right(Equilateral) )
		}
	}

	test("Categorizing Triangle having 3 positive sides with 2 sides only equal should return Isosceles") {
		forAll( positive2SidesOnlySame ) { triangleSides => 
			val (side1, side2, side3) = triangleSides
			assert( triangleType(side1, side2, side3) === Right(Isosceles) )
		}
	}

	test("Categorizing Triangle having 3 positive sides with 3 distinct sizes return Scalene") {
		forAll( positive3UniqueSides ) { triangleSides => 
			val (side1, side2, side3) = triangleSides
			assert( triangleType(side1, side2, side3) === Right(Scalene) )
		}
	}

	test("Finding 5th from last list's elements should 'fail' when List.size <= 4") {
	    forAll(randomSizedListMaxSizeN(arbitrary[Int], 4)) { list => 
	      assert( fifthFromLast(list) === None)
	      assert( fifthFromLastAlternative(list) === None)
	    }
    }	

	test("Finding 5th from last list's elements should always succeed when List.size > 5") {
	    forAll(randomListSizeMinSizeN(arbitrary[Int], 5)) { list => 
	      assert( fifthFromLast(list).isDefined )
	      assert( fifthFromLastAlternative(list).isDefined )
	    }
    }	

	test("Finding 5th from last list's elements function should always match List.reverse.drop(4).take(1).headOption") {
	    forAll(Gen.listOf(arbitrary[String])) { list => 
	      val expected = list.reverse.drop(4).take(1).headOption
	      assert( fifthFromLast(list) ===  expected)
	      assert( fifthFromLastAlternative(list) === expected )
	    }
    }	

    test("Two lists with the same elements (but in shuffled) order should return true for `sameElements`") {
    	forAll(Gen.listOf(arbitrary[Long])) { list =>
    		val shuffled: List[Long] = scala.util.Random.shuffle( list )
    		assert( sameElements(list, shuffled) )
    	}
    }

    val PadByOneElem: String = "foobar"

    test("Two lists varying by 1-element should return false for `sameElements`") {
    	forAll( Gen.listOf(arbitrary[String]) ) { list => 
    		val addedElement: List[String] = PadByOneElem :: list 
    		assert( sameElements(list, addedElement) == false )
    	}
    }

}