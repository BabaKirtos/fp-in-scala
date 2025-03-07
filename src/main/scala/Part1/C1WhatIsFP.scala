package Part1

import scala.annotation.tailrec

object C1WhatIsFP extends App {

  trait Payments {
    // We are forced to create an interface/trait of Payments for testing
    // Whereas a concrete class would have been enough
    // This introduces additional unwanted complexity
    def charge(cc: CreditCard, amount: Int): Unit =
      println(s"Successfully charged credit card ${cc.number} for $$$amount")
  }

  class CreditCard(val number: Int) {
    def charge(price: Int): Unit =
      println(s"Successfully charged credit card $number for $$$price")
  }

  class Coffee {
    def price: Int = 10
  }

  // A case class has one primary constructor whose argument list comes after
  // the class name (here, Charge).
  // The parameters in this list become public,
  // unmodifiable (immutable) fields of the class and can be accessed using
  // the usual object-oriented dot notation, as in other.cc.
  case class Charge(cc: CreditCard, amount: Int) {
    // We won't be calling the below method as it is a side effect
    def executeCharge(): Unit = cc.charge(amount)

    // The book's implementation to combine charges
    def combine(other: Charge): Charge = {
      if (cc == other.cc)
        Charge(cc, amount + other.amount)
      else
        throw new Exception("Can't combine charges to different cards")
    }
  }

  class Cafe {
    def buyCoffeeOld(cc: CreditCard): Coffee = {
      val cup = new Coffee
      cc.charge(cup.price) // this is a side effect
      cup
    }

    // let's try to remove the above side effect
    def buyCoffeeNew(cc: CreditCard, p: Payments): Coffee = {
      val cup = new Coffee
      // we still need to mock Payments to test this
      // Also, what will happen if multiple coffees are ordered
      // For every charge we will have to pay a processing fee
      p.charge(cc, cup.price)
      cup
    }

    // How do we create a purely functional method?
    // We need to get away with the side effects
    // We can return Coffee along with Charge
    // Below is my implementation
    def buyCoffeesFunctional(cc: CreditCard, numCups: Int = 0): (List[Coffee], Charge) = {
      @tailrec
      def cups(counter: Int, acc: List[Coffee] = Nil): List[Coffee] = {
        if (counter == 0) acc
        else cups(counter - 1, List(new Coffee) ++ acc)
      }

      val totalCoffees: List[Coffee] = cups(numCups)
      val totalPrice = coalesce(totalCoffees)
      (totalCoffees, Charge(cc, totalPrice))
    }

    def coalesce(numCoffees: List[Coffee], acc: Int = 0): Int = {
      if (numCoffees == Nil) acc
      else coalesce(numCoffees.tail, acc + numCoffees.head.price)
    }

    // Below are the book's implementations
    def buyCoffeeBook(cc: CreditCard): (Coffee, Charge) = {
      val cup = new Coffee
      (cup, Charge(cc, cup.price))
    }

    def buyCoffeesBook(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
      // Note how the book s using in-built methods to create and
      // manipulate lists, whereas we create our own tail-recursive functions
      val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffeeBook(cc))
      val (coffees, charges) = purchases.unzip
      (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
    }
  }

  // The book's implementation to coalesce charges
  def coalesceBook(charges: List[Charge]): List[Charge] =
    // This is very well written to combine charges with different credit cards
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList

  val testOrder = new Cafe
  testOrder.buyCoffeeOld(new CreditCard(123))
  testOrder.buyCoffeeNew(new CreditCard(456), new Payments {})
  val (coffees, charge) = testOrder.buyCoffeesFunctional(new CreditCard(789), 3)
  println(coffees)
  println(charge.amount)

  // testing coalesceBook method
  val differentCharges: List[Charge] = List(
    Charge(new CreditCard(123), 20),
    Charge(new CreditCard(123), 30),
    Charge(new CreditCard(456), 10)
  )

  println(coalesceBook(differentCharges).toString())
}
