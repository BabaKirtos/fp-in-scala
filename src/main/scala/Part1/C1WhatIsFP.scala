package Part1

object C1WhatIsFP extends App {

  trait Payments {
    def charge(cc: CreditCard, amount: Int): String = "Success"
  }

  class CreditCard {
    def charge(price: Int): String = "Success"
  }

  class Coffee {
    def price: Int = 10
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

    // How do we create a purely functional method
    // We need to get away the side effects
//    def buyCoffeeFunctional()
  }
}
