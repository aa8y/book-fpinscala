package co.aa8y.book.fpinscala

import org.scalatest.FlatSpec

class Chapter3Spec extends FlatSpec {
  val testList = List(1, 2, 3, 4)

  "tail()" should "return everything but the first element." in {
    assert(List.tail(testList) === List(2, 3, 4))
  }

  it should "throw an exception when called on an empty list." in {
    intercept[UnsupportedOperationException] {
      List.tail(Nil)
    }
  }

  "setHead()" should "return everything but the first element." in {
    assert(List.setHead(testList, -1) === List(-1, 2, 3, 4))
  }
}
