package Part1

import scala.annotation.tailrec

object C2GettingStartedFP {

  /*
  What are side effects? A function has a side effect if it does something other than simply return a result, for example:
   Modifying a variable
   Modifying a data structure in place
   Setting a field on an object
   Throwing an exception or halting with an error  Printing to the console or reading user input
   Reading from or writing to a file
   Drawing on the screen
   */

  // A comment!
  /* Multi-line
 comment */

  /** A documentation comment */

  // polymorphic HOFs are where we use types to guide us toward an implementation
  /* The object keyword creates a new singleton type,
  which is like a class that only has a single named instance.*/
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    // everything in scala is an object, including the val msg below
    // which is a String object
    val msg = "The absolute of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  // Fibonacci series
  // F0 = 0, F1 = 1, F2 = 1, F3 = 2, F4 = 3, F5 = 5
  def fibonacci(n: Int): Int = {
    @tailrec
    def fibTailRec(n: Int, a: Int, b: Int): Int = {
      if (n == 0) a
      else fibTailRec(n - 1, b, a + b)
    }

    fibTailRec(n, 0, 1)
  }

  private def formatFibonacci(n: Int) = {
    val msg = "The fibonacci of %d is %d."
    msg.format(n, fibonacci(n))
  }

  // All the above format methods are similar, 
  // So we can replace them with a HoF
  private def formatResult(s: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(s, n, f(n))
  }

  /*
  * Often, and especially when writing HOFs, we want to write code
  * that works for any type it’s given. These are called polymorphic functions.
  * */
  def findFirstIndex[A](data: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int =
      if (n >= data.length) -1
      else if (p(data(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(index: Int): Boolean = {
      if (index >= as.length - 1) true
      else if (!ordered(as(index), as(index + 1))) false
      else loop(index + 1)
    }

    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  // our main method is an outer shell that calls into our purely functional core
  def main(args: Array[String]): Unit = {

    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatFibonacci(10))

    println(formatResult("absolute", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibonacci", 10, fibonacci))

    println(findFirstIndex(Array(0, 1, 2, 3, 4), _ % 2 == 0))
    println(findFirstIndex(Array("Hi", "Bye", "wifi", "Neel", "Babu"), _.contains("Babu")))

    // testing isSorted
    val arr1 = Array(1, 2, 3, 4, 5)
    val arr2 = Array(5, 4, 3, 2, 1)

    println(isSorted(arr1, _ <= _)) // Output: true
    println(isSorted(arr1, _ >= _)) // Output: false
    println(isSorted(arr2, _ >= _)) // Output: true
    println(isSorted(arr2, _ <= _)) // Output: false
  }

  /*
  * The implementations of members within an object can refer to
  * each other unqualified (without prefixing the object name), but
  * if needed, they have access to their enclosing object using a special name: this
  * */

  /*
  * The first new idea is this: functions are values.
  * And just like values of other types—such as integers, strings,
  * and lists—functions can be assigned to variables, stored in data structures,
  * and passed as arguments to functions.
  * */
}
