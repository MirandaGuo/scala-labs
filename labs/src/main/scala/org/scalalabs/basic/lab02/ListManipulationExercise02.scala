package org.scalalabs.basic.lab02

import scala.collection.mutable.ListBuffer
import sys._

object ListManipulationExercise02 {

  /**
   * Find the maximum element in a list, e.g. maxElementInList(List(1,9,3,5)) == 9
   * As usual, various ways exist: pattern matching, folding, ...
   */
  def maxElementInList(l: List[Int]): Int = {
    l.max
  }

  /**
   * Calculate the sum of the equally position elements
   * of the two list
   */
  def sumOfTwo(l1: List[Int], l2: List[Int]): List[Int] = {
   val seq: List[(Int, Int)] =  l1 zipAll(l2, 0, 0)
    seq.map(element => {
      element._1 + element._2
    })

  }

  /**
   *  For this exercise preferably make use of the sumOfTwo
   * method above
   */
  def sumOfMany(l: List[Int]*): List[Int] = {
      l.foldLeft(List(0))((r, c) => sumOfTwo(r, c))
  }


  case class Person(age: Int, firstName: String, lastName: String)

  /**
   * The following method is implemented in the most in-elegant way we could think of.
   * The idea is to re-write the method into more functional style. In the end, you
   * may be able to achieve the same functionality as implemented below
   * in a one-liner.
   */
  def separateTheMenFromTheBoys(persons: List[Person]): List[List[String]] = {

    val (boys, men) = persons.partition( _.age < 18)

    List(boys.sortBy(_.age).map(_.firstName), men.sortBy(_.age).map(_.firstName))
  }

}