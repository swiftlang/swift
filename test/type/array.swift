// RUN: %swift -parse %s -verify

// Array types.
class Base1 {
  func f0(x: [Int]) { }
  func f1(x: [[Int]]) { }
  func f2(x: [[Int] -> [Int]]) { }
}

class Derived1 : Base1 {
  override func f0(x: Array<Int>) { }
  override func f1(x: Array<Array<Int>>) { }
  override func f2(x: Array<Array<Int> -> Array<Int>>) { }
}


// Array types in generic specializations.
struct X<T> { }

func testGenericSpec() {
  let x = X<[Int]>()
}
