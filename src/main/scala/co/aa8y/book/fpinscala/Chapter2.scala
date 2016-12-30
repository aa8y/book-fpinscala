package co.aa8y.book.fpinscala

import scala.annotation.tailrec

object Chapter2 {
  // Exercise 2.1
  def fib(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException(s"n cannot be less than 0. $n was passed.")
    if (n <= 1) n
    else fib(n - 2) + fib(n - 1)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int, nPlus1: Int): Boolean = {
      if (nPlus1 > as.size - 1) true
      else ordered(as(n), as(nPlus1)) && loop(n + 1, nPlus1 + 1)
    }
    if (as.size <= 1) true
    else loop(0, 1)
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => {
      (b: B) => f(a, b)
    }
  }

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
