package com

import com.Exercises._
import com.AST._
import org.scalatest._

class ExercisesSpec extends FunSpec {
    describe("Testing 'bag' functionality") {
    	it("should return an empty Map for an empty List") {
    		assert(bag[Int](Nil) === Map.empty[Int, Int])
    	}

    	it("should return a Map capturing case sensitivity") {
    		val list = List("the", "fox", "jumped", "over", "THE", "fox")
    		val result: Map[String, Int] = bag(list)
    		val expected = Map("the" -> 1, "fox" -> 2, "jumped" -> 1, "over" -> 1, "THE" -> 1)
    		assert(result == expected)
    	}

    	it("should return a Map capturing positive and negative numbers") {
    		val list = List(-1, 1, 0, 1, 1)
    		val result: Map[Int, Int] = bag(list)
    		val expected = Map(-1 -> 1, 0 -> 1, 1 -> 3)
    		assert(result == expected)
    	}
    }
}