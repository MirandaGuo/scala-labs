package org.scalalabs.basic.lab01

import scala.language.implicitConversions

/**
 * The goal of this exercise is to get familiar basic OO constructs in scala
 *
 * Fix the code so that the unit test 'CurrencyExerciseTest' passes.
 *
 * In order for the tests to pass you need to do the following:
 *
 * Exercise 1:
 * - Create a class Euro
 * - Provide it with two constructor parameters: euro:Int, cents:Int
 * - Provide the cents field with default value: 0
 * - Provide an immutable field named: inCents that converts euro + cents into cents.
 * - Create an object Euro with a factory method named: fromCents that creates an Euro based on cents.
 * - Create a method named: + to the Euro class that adds another Euro
 * - Create a method named: * to the Euro class that multiplies an Euro
 *
 * Exercise 2:
 * - Create an abstract class Currency
 * - Provide it with one constructor parameter: symbol:String
 * - Extend the previously created Euro class from Currency
 * - Override the toString method of Euro to represent the following String:
 * -> symbol + ': ' + euro + ',' + cents.  E.g: EUR 200,05
 * - In case the cents are 0 use this representation:
 * -> symbol + ': ' + euro + ',--. E.g.: EUR 200.--
 *
 * Exercise 3:
 * - Mix the Ordered trait in Euro
 * - Implement the compare method
 *
 * Exercise 4:
 * - Provide an implicit class that adds a *(euro:Euro) method to Int
 * - Create a new currency Dollar
 * - Provide a implicit conversion method that converts from Euro to Dollar using the
 * [[org.scalalabs.basic.lab01.DefaultCurrencyConverter]]
 *
 * Exercise 5:
 * - Extend the conversion method from Euro to Dollar with an implicit parameter
 * of type [[org.scalalabs.basic.lab01.CurrencyConverter]]
 * - Use the implicit CurrencyConverter to do the conversion.
 */
object Euro {

  def fromCents(cents: Int): Euro = {
        val euro = cents / 100
        val cent = cents % 100
        new Euro(euro, cent)
  }

  implicit class IntWithEuro(factor: Int) {
    def *(euro:Euro): Euro = euro * factor
  }

  implicit def DollarToEuro(dollar: Dollar)(implicit defaultConvert: CurrencyConverter): Euro = fromCents(defaultConvert.toEuroCents(dollar.inCents))

}


class Euro(val euro: Int, val cents: Int = 0)(override implicit val symbol: String = "EUR") extends Currency(symbol) with Ordered[Euro] {

  val inCents: Int = (euro / 0.01).toInt + cents

  def +(that: Euro): Euro = Euro.fromCents(this.inCents + that. inCents)


  def *(that: Euro): Euro = Euro.fromCents(this.inCents * that.inCents)

  def *(factor: Int): Euro = Euro.fromCents(this.inCents * factor)

  def /(divider:Int): Euro = {
    require(divider > 0)

    Euro.fromCents(this.inCents / divider)
  }

  override def toString(): String = {
    if (cents != 0)
      symbol + ": " + euro + "," +  (cents.toDouble / 100).toString().takeRight(2)
    else
      symbol + ": " + euro + ",--"
  }

  override def compare(that: Euro) = {
    if (this.inCents > that.inCents) 1
    else if (this.inCents == that.inCents) 0
    else -1
  }

}

abstract class Currency(val symbol: String) {

}

class Dollar(val euro: Int, val cents: Int) extends Currency("USD"){
  val inCents: Int = (euro / 0.01).toInt + cents



}






