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

  class Charge(val cc: CreditCard, val price: Int) {
    // We won't be calling the below method as it is a side effect
    def executeCharge(): Unit = cc.charge(price)
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
    def buyCoffeesFunctional(cc: CreditCard, numCups: Int = 0): (List[Coffee], Charge) = {
      @tailrec
      def cups(counter: Int, acc: List[Coffee] = Nil): List[Coffee] = {
        if (counter == 0) acc
        else cups(counter - 1, List(new Coffee) ++ acc)
      }

      val totalCoffees: List[Coffee] = cups(numCups)
      val totalPrice = coalesce(totalCoffees)
      (totalCoffees, new Charge(cc, totalPrice))
    }

    def coalesce(numCoffees: List[Coffee], acc: Int = 0): Int = {
      if (numCoffees == Nil) acc
      else coalesce(numCoffees.tail, acc + numCoffees.head.price)
    }
  }

  val testOrder = new Cafe
  testOrder.buyCoffeeOld(new CreditCard(123))
  testOrder.buyCoffeeNew(new CreditCard(456), new Payments {})
  val (coffees, charge) = testOrder.buyCoffeesFunctional(new CreditCard(789), 3)
  println(coffees)
  println(charge.price)
}
