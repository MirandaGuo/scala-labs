package org.scalalabs.basic.lab03
import sys._
/**
 * This exercise introduces you to the powerful pattern matching features of Scala.
 *
 * Pattern matching can in its essence be compared to Java's 'switch' statement,
 * even though it provides many more possibilites. Whereas the Java switch statmenet
 * lets you 'match' primitive types up to int's, Scala's pattern matching goes much
 * further. Practically everything from all types of objects and Collections
 * can be matched, not forgetting xml and a special type of class called case classes.
 *
 * Pattern matching is also often used in combination with recursive algorithms.
 *
 * For this exercise exclusively use pattern matching constructs in order to make the
 * corresponding unit test work.
 *
 * Reference material to solve these exercises can be found here:
 * Pattern matching in general: http://programming-scala.labs.oreilly.com/ch03.html#PatternMatching
 * Pattern matching in combination with partial functions: http://programming-scala.labs.oreilly.com/ch08.html#PartialFunctions
 */

object PatternMatchingExercise {

  /**
   * ***********************************************************************
   *  pattern matching exercises
   * For expected solution see unittest @PatternMatchingExerciseTest
   * ***********************************************************************
   */

  def describeLanguage(s: String) = s match {
    case "Java" => "OOP"
    case "Smalltalk" => "OOP"
    case "Clojure" => "Functional"
    case "Haskell" => "Functional"
    case "Scala" => "Hybrid"
    case "C" => "Procedural"
    case _ => "Unknown"

  }

  def matchOnInputType(in: Any) = in match {
    case x: String => "A string with length " + x.length
    case x: Int if(x >= 0) => "A positive integer"
    case x: Int if(x < 0) => "A negative integer"
    case x: Seq[Any] if x.length > 10 => "Seq with more than 10 elements"
    case Seq("first", "second", "third", "fourth") => "first: first, second: second, rest: List(third, fourth)"
    case x: Person => "A person with name: " + x.name
    case Some(_) => "A Scala Option subtype"
    case None => "A Scala Option subtype"
    case null => "A null value"
    case _ => "Some Scala class"
  }

  def older(p: Person): Option[String] = p match {
    case man: Person if man.age > 30 => Some(man.name)
    case man: Person if man.age <=30 => None
  }

  /**
   * ***********************************************************************
   * Pattern matching with partial functions
   * For expected solution see @PatternMatchingExerciseTest
   * ***********************************************************************
   */


  val pf1: PartialFunction[String, String] = {
    case "scala-labs" => "scala-labs"
    case "stuff" => "stuff"
  }

  val pf2: PartialFunction[String, String] = {
    case "other stuff" => "other stuff"
  }

  val pf3: PartialFunction[String, String] = {
    case "scala-labs" => "scala-labs"
    case "stuff" => "stuff"
    case "other stuff" => "other stuff"
  }

}

case class Person(name: String, age: Int)