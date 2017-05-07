package co.aa8y.book.fpinscala

import scala.annotation.tailrec

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

  // Exercise 3.5
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => throw new UnsupportedOperationException(s"Cannot drop elements from an empty list.")
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else t
  }

  def append[A](l: List[A], r: List[A]): List[A] = l match {
    case Nil => r
    case Cons(h, t) => Cons(h, append(t, r))
  }

  // Exercise 3.6
  def init[A](as: List[A]): List[A] = {
    @tailrec
    def loop(as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc match {
        case Nil => throw new UnsupportedOperationException("No element to drop in an empty list.")
        case _ => acc
      }
      case Cons(h, t) => loop(t, append(acc, Cons(h, Nil)))
    }
    loop(as, Nil)
  }
}
