package Part1

import scala.annotation.tailrec

object C3FunctionalDataType extends App {

  sealed trait LinkedList[+A]

  case object Empty extends LinkedList[Nothing]

  case class Cons[+A](h: A, t: LinkedList[A]) extends LinkedList[A] {

    def sumRecursive(ints: LinkedList[Int]): Int = ints match
      case Empty => 0
      case Cons(h, t) => h + sumRecursive(t) // recursive not tail recursive

    def sum(ints: LinkedList[Int]): Int = {
      @tailrec
      def loop(ints: LinkedList[Int], acc: Int): Int = ints match
        case Empty => acc
        case Cons(h, t) => loop(t, acc + h) // tail recursive

      loop(ints, 0)
    }
  }

  val testList = new Cons[Int](2, new Cons[Int](5, new Cons[Int](8, Empty)))
  println(testList.sum(testList))
}

