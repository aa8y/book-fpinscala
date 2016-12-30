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
}
