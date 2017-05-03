package co.aa8y.book.fpinscala

import scala.annotation.tailrec

object Chapter2 {
  // Exercise 2.1
  def naivefib(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException(s"n cannot be less than 0. $n was passed.")
    if (n <= 1) n
    else fib(n - 2) + fib(n - 1)
  }

  def fib(n: Int): Int = {
    if (n < 0) throw new IllegalArgumentException(s"n cannot be less than 0. $n was passed.")
    @tailrec
    def loop(count: Int, p: Int, q: Int): Int = {
      if (count >= n) p
      else loop(count + 1, q, p + q)
    }
    loop(0, 0, 1)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(m: Int, n: Int): Boolean = {
      if (n == as.size) true
      else if (!ordered(as(m), as(n))) false
      else loop(n, n + 1)
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
