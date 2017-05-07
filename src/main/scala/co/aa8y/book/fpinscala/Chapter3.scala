package co.aa8y.book.fpinscala

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new UnsupportedOperationException("Cannot find tail of an empty list.")
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](as: List[A], head: A): List[A] = Cons(head, tail(as))

  // Exercise 3.4
  def drop[A](as: List[A], n: Int): List[A] = n match {
    case 0 => as
    case _ =>
      if (n < 0) throw new IllegalArgumentException(s"n, given $n, cannot be less than 0.")
      else as match {
        case Nil =>
          throw new UnsupportedOperationException(s"Cannot drop $n elements from an empty list.")
        case Cons(h, t) => drop(t, n - 1)
      }
  }
}
