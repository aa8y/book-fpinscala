package co.aa8y.book.fpinscala

import org.scalatest.FlatSpec

class Chapter2Spec extends FlatSpec {
  import Chapter2._

  "fib()" should "fail for a value less than 0." in {
    intercept[IllegalArgumentException] {
      fib(-1)
    }
  }

  it should "return the sum of the previous two." in {
    assert(fib(0) === 0)
    assert(fib(1) === 1)
    assert(fib(2) === 1)
    assert(fib(3) === 2)
    assert(fib(4) === 3)
    assert(fib(5) === 5)
    assert(fib(6) === 8)
  }

  "isSorted()" should "check if an array is sorted based on the comparison function." in {
    assert(isSorted(Array(1, 4, 5), (x: Int, y: Int) => x <= y) === true)
    assert(isSorted(Array(4, 5, 3), (x: Int, y: Int) => x <= y) === false)
    assert(isSorted(Array(5, 3, 1), (x: Int, y: Int) => x >= y) === true)
    assert(isSorted(Array("foo", "bar", "baz"), (x: String, y: String) => x <= y) === false)
    assert(isSorted(Array("bar", "baz", "foo"), (x: String, y: String) => x <= y) === true)
  }

  it should "always return true for an empty array or an array." in {
    assert(isSorted(Array.empty, (x: Int, y: Int) => x <= y) === true)
    assert(isSorted(Array(1), (x: Int, y: Int) => false) === true)
  }

  "curry()" should "curry." in {
    val input = (c: Char, i: Int) => (c + i).toChar.toString
    val output = curry(input)
    assert(input('a', 1) === output('a')(1))
  }

  "uncurry()" should "undo a curry." in {
    val input = (c: Char) => { (i: Int) => (c + i).toChar.toString }
    val output = uncurry(input)
    assert(input('a')(1) === output('a', 1))
  }

  "compose()" should "compose two functions by combining them." in {
    val incrementChar = (c: Char) => (c + 1)
    val intAsCharString = (i: Int) => i.toChar.toString
    val nextCharAsString = compose(intAsCharString, incrementChar)
    assert(nextCharAsString('a') === "b")
  }
}
