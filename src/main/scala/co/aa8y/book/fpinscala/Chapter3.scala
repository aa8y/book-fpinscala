package co.aa8y.book.fpinscala

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  private def nilNoOp(op: String) = new UnsupportedOperationException(s"Cannot $op an empty list.")

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw nilNoOp("find tail of")
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](as: List[A], head: A): List[A] = Cons(head, tail(as))

  // Exercise 3.4
  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = n match {
    case 0 => as
    case _ =>
      if (n < 0) throw new IllegalArgumentException(s"n, given $n, cannot be less than 0.")
      else as match {
        case Nil if (n > 0) => 
          val msg = s"n, given $n, cannot be greater than the size of the list."
          throw new IllegalArgumentException(msg)
        case Cons(h, t) => drop(t, n - 1)
      }
  }

  def dropRight[A](as: List[A], n: Int): List[A] = reverse(drop(reverse(as), n))

  // Exercise 3.5
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else as
    case _ => as
  }

  def append[A](l: List[A], r: List[A]): List[A] = l match {
    case Nil => r
    case Cons(h, t) => r match {
      case Nil => l
      case _ => Cons(h, append(t, r))
    }
  }

  // Exercise 3.6
  def init[A](as: List[A]): List[A] = reverse(tail(reverse(as)))

  def reverse[A](as: List[A]): List[A] = {
    @tailrec
    def loop(as: List[A], acc: List[A]): List[A] = as match {
      case Nil => acc
      case Cons(h, t) => loop(t, Cons(h, acc))
    }
    loop(as, Nil)
  }

  def last[A](as: List[A]): A = as match {
    case Nil => throw nilNoOp("find the last element of")
    case Cons(h, Nil) => h
    case Cons(_, t) => last(t)
  }
}
