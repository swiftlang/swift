// RUN: %target-parse-verify-swift

// Array types.
class Base1 {
  func f0(x: [Int]) { }
  func f0a(x: [Int]?) { }
  func f1(x: [[Int]]) { }
  func f1a(x: [[Int]]?) { }
  func f2(x: [[Int] -> [Int]]) { }
  func f2a(x: [[Int]? -> [Int]?]?) { }
}

class Derived1 : Base1 {
  override func f0(x: Array<Int>) { }
  override func f0a(x: Optional<Array<Int>>) { }
  override func f1(x: Array<Array<Int>>) { }
  override func f1a(x: Optional<Array<Array<Int>>>) { }
  override func f2(x: Array<Array<Int> -> Array<Int>>) { }
  override func f2a(x: Optional<Array<Optional<Array<Int>> -> Optional<Array<Int>>>>) { }
}


// Array types in generic specializations.
struct X<T> { }

func testGenericSpec() {
  _ = X<[Int]>()
}

// Array types for construction.
func constructArray(n: Int) {
  var ones = [Int](count: n, repeatedValue: 1)
  ones[5] = 0

  var matrix = [[Float]]()
  matrix[1][2] = 3.14159

  var _: [Int?] = [Int?]()
}

// Fix-Its from the old syntax to the new.

typealias FixIt0 = Int[] // expected-error{{array types are now written with the brackets around the element type}}{{20-20=[}}{{23-24=}}
typealias FixIt1 = Int[][] // expected-error{{array types are now written with the brackets around the element type}}{{20-20=[}}{{25-26=}}
// expected-error@-1{{array types are now written with the brackets around the element type}}{{20-20=[}}{{23-24=}}

