package co.aa8y.book.fpinscala

import scala.annotation.tailrec

object Chapter2 {
  // Exercise 2.1
  def fib(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException(s"n cannot be less than 0. $n was passed.")
    if (n <= 1) n
    else fib(n - 2) + fib(n - 1)
  }
}
