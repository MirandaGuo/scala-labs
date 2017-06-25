package org.scalalabs.basic.lab04

import org.joda.time.{DateTime, Duration}

import scala.math._
import language.implicitConversions
import language.higherKinds
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.scalalabs.basic.lab04.Exercise03.{EuroJsonMarshallerHelper, JsonConverter}

import scala.util.control._

case class Euro(val euros: Int, val cents: Int) {
  lazy val inCents: Int = euros * 100 + cents
}

object Euro {
  def fromCents(cents: Int) = new Euro(cents / 100, cents % 100)

  implicit object EuroAsJsonConverter extends JsonConverter[Euro] {
    override def toJSON(t: Euro): JValue = EuroJsonMarshallerHelper.marshal(t)

    override def fromJson(json: JValue): Euro = EuroJsonMarshallerHelper.unmarshal(json)
  }
}

/**
 * Exercise 1:
 * Create a money DSL which allows you to create Euro classes as follows:
 * - 2 euros           => Euro(2, 0)
 * - 40 cents          => Euro(0, 40)
 * - 2 euros 45 cents  => Euro(2,45)
 * The Euro case class is already provided.
 * Hint: Use an intermediate class (e.g. EuroBuilder) to create the Euro object.
 * E.g. 2 euros = 2 -> EuroBuilder
 * Use an implicit conversion from EuroBuilder -> Euro to get the final result
 * In the EuroBuilder you might need the apply() method to cover this case:
 * 2 euros >45< cents
 */
object Exercise01 {

  class EuroBuilder(val inCents: Int) {

     def apply(eb: EuroBuilder) = new EuroBuilder(inCents + eb.inCents)

     def euros = new EuroBuilder(inCents * 100)

     def cents = new EuroBuilder(inCents)

  }

  implicit def convertToEuroBuilder(num: Int): EuroBuilder = {
      new EuroBuilder(num)
  }

  implicit def convertToEuro(eb: EuroBuilder): Euro = {
     Euro.fromCents(eb.inCents)
  }


}

/**
 * Exercise 2:
 * Implement Scala's built in Ordering type class for Euro,
 * so that the call to Seq(Euro(1,5), Euro(3,2)).sorted compiles.
 */
object Exercise02 {

    implicit object OrderedEuro extends Ordering[Euro] {
       def compare(x: Euro, y: Euro): Int = x.inCents - y.inCents
    }

}

/**
 * Exercise 3:
 * Implement a type class pattern to convert domain objects to and from json.
 * Take a look at the already defined type class trait @see JsonConverter.
 * 1. Implement the methods of the JsonCoverter object below that converts domain objects to and from json making use of the JsonConverter type class trait.
 * 2. Provide an implementation of the JsonConverter type class trait for the Euro class.
 * Place the implementation in the Euro's companion object so that the implicit resolution requires no import.
 * For marshalling and unmarshalling json make use of the @see EuroJsonMarshallerHelper
 */
object Exercise03 {
  object JsonConverter {
    def convertToJson[T : JsonConverter](t: T): JValue = {
               implicitly[JsonConverter[T]].toJSON(t)

    }
    def parseFromJson[T : JsonConverter ](json: JValue): T = {
       implicitly[JsonConverter[T]].fromJson(json)
    }
  }

  /**
   * Only used for Exercise03!
   */
  trait JsonConverter[T] {
    def toJSON(t: T): JValue
    def fromJson(json: JValue): T
  }
  /**
   * Only used for Exercise03!
   */
  object EuroJsonMarshallerHelper {
    implicit val formats = DefaultFormats
    def marshal(e: Euro): JValue = ("symbol" -> "EUR") ~ ("amount" -> s"${e.euros},${e.cents}")
    def unmarshal(json: JValue): Euro = {
      Exception.allCatch.opt {
        val amount = (json \ "amount").extract[String].split(",")
        Euro(amount(0).toInt, amount(1).toInt)
      } getOrElse (Euro(0, 0))
    }
  }
}

