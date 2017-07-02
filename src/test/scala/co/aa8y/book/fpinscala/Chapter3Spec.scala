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
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x % 2 == 1) === List(2, 3, 4))
  }

  it should "return the same list if the criteria is never met." in {
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x > 10) === List(1, 2, 3, 4))
  }

  it should "return Nil if all elements meet the criteria." in {
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 10) === Nil)
  }

  "setHead()" should "return everything unchanged but the first element." in {
    assert(setHead(testList, -1) === List(-1, 2, 3, 4))
  }

  it should "throw an exception when invoked on an empty list." in {
    intercept[UnsupportedOperationException] {
      setHead(Nil, 10)
    }
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

  "reverse()" should "reverse the contents of a list." in {
    assert(reverse(List(1, 2, 3, 4)) === List(4, 3, 2, 1))
  }

  it should "return Nil for a Nil list." in {
    assert(reverse(Nil) === Nil)
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
