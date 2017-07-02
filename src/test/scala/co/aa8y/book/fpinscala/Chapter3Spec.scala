package co.aa8y.book.fpinscala

import org.scalatest.FlatSpec

class Chapter3Spec extends FlatSpec {
  import List._

  val testList = List(1, 2, 3, 4)

  "append()" should "append the elements of the right list to the end of the left list." in {
    assert(append(List(1, 2), Nil) === List(1, 2))
    assert(append(Nil, List(3, 4)) === List(3, 4))
    assert(append(List(1, 2), List(3, 4)) === List(1, 2, 3, 4))
  }

  it should "return Nil if both the left and right lists are Nil." in {
    assert(append(Nil, Nil) === Nil)
  }

  "drop()" should "drop first n elements of a list." in {
    assert(drop(List(1, 2, 3, 4), 2) === List(3, 4))
  }

  it should "return the same list if n is 0." in {
    assert(drop(List(1, 2, 3, 4), 0) === List(1, 2, 3, 4))
  }

  it should "return Nil if n == size of the list." in {
    assert(drop(List(1, 2, 3, 4), 4) === Nil)
  }

  it should "throw an exception if n > size of the list." in {
    intercept[IllegalArgumentException] {
      drop(List(1, 2, 3, 4), 6)
    }
  }

  it should "throw an exception if n < 0." in {
    intercept[IllegalArgumentException] {
      drop(List(1, 2, 3, 4), -1)
    }
  }

  "dropRight()" should "drop last n elements of a list." in {
    assert(dropRight(List(1, 2, 3, 4), 2) === List(1, 2))
  }

  it should "return the same list if n is 0." in {
    assert(dropRight(List(1, 2, 3, 4), 0) === List(1, 2, 3, 4))
  }

  it should "return Nil if n == size of the list." in {
    assert(dropRight(List(1, 2, 3, 4), 4) === Nil)
  }

  it should "throw an exception if n > size of the list." in {
    intercept[IllegalArgumentException] {
      dropRight(List(1, 2, 3, 4), 6)
    }
  }

  it should "throw an exception if n < 0." in {
    intercept[IllegalArgumentException] {
      dropRight(List(1, 2, 3, 4), -1)
    }
  }

  "dropWhile()" should "stop dropping elements as soon as the criteria is not met." in {
    assert(dropWhile(List(1, 2, 3, 4))(_ % 2 == 1) === List(2, 3, 4))
  }

  it should "return the same list if the criteria is never met." in {
    assert(dropWhile(List(1, 2, 3, 4))(_ > 10) === List(1, 2, 3, 4))
  }

  it should "return Nil if all elements meet the criteria." in {
    assert(dropWhile(List(1, 2, 3, 4))(_ < 10) === Nil)
  }

  "foldLeft()" should "start folding from left to right." in {
    assert(foldLeft(List(1f, 2f, 4f), 8f)((acc, x) => acc / x) === 1f)
  }

  it should "return the list reversed when the aggregating function is the list constructor." in {
    assert(foldLeft(List(1, 2, 3), Nil: List[Int])((acc, x) => Cons(x, acc)) === List(3, 2, 1))
  }

  "foldRight()" should "start folding from right to left and have the same result as foldLeft." in {
    assert(foldRight(List(1f, 2f, 4f), 8f)((x, acc) => acc / x) === 1f)
  }

  it should "return the same list back when the aggregating function is the list constructor." in {
    assert(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) === List(1, 2, 3))
  }

  "init()" should "return all but the last element in the list." in {
    assert(init(List(1, 2, 3, 4)) === List(1, 2, 3))
  }

  it should "throw an exception when invloked on an empty list." in {
    intercept[UnsupportedOperationException] {
      init(Nil)
    }
  }

  "last()" should "return the last element in a list." in {
    assert(last(List(1, 2, 3, 4)) === 4)
  }

  it should "throw an exception when invoked on an empty list." in {
    intercept[UnsupportedOperationException] {
      last(Nil)
    }
  }

  "length()" should "return the size of the list." in {
    assert(length(List(1, 2, 3)) === 3)
    assert(length(Nil) === 0)
  }

  "productLeft()" should "compute the product of all the numbers in the list." in {
    assert(productLeft(List(2d, 3d, 4d)) === 24d)
    assert(productLeft(List(2d, 0d, 4d)) === 0d)
  }

  it should "return 1.0 for an empty list." in {
    assert(productLeft(Nil) === 1d)
  }

  "productRight()" should "compute the product of all the numbers in the list." in {
    assert(productRight(List(2d, 3d, 4d)) === 24d)
    assert(productRight(List(2d, 0d, 4d)) === 0d)
  }

  it should "return 1.0 for an empty list." in {
    assert(productRight(Nil) === 1d)
  }

  "reverse()" should "reverse the contents of a list." in {
    assert(reverse(List(1, 2, 3, 4)) === List(4, 3, 2, 1))
  }

  it should "return Nil for a Nil list." in {
    assert(reverse(Nil) === Nil)
  }

  "setHead()" should "return everything unchanged but the first element." in {
    assert(setHead(testList, -1) === List(-1, 2, 3, 4))
  }

  it should "throw an exception when invoked on an empty list." in {
    intercept[UnsupportedOperationException] {
      setHead(Nil, 10)
    }
  }

  "sumLeft()" should "compute the sum of all integers in the list." in {
    assert(sumLeft(List(1, 2, 3)) === 6)
  }

  it should "return 0 for an empty list." in {
    assert(sumLeft(Nil) === 0)
  }

  "tail()" should "return everything but the first element." in {
    assert(tail(testList) === List(2, 3, 4))
  }

  it should "throw an exception when invoked on an empty list." in {
    intercept[UnsupportedOperationException] {
      tail(Nil)
    }
  }
}
