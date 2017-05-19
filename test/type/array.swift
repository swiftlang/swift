// RUN: %target-typecheck-verify-swift

// Array types.
class Base1 {
  func f0(_ x: [Int]) { }
  func f0a(_ x: [Int]?) { }
  func f1(_ x: [[Int]]) { }
  func f1a(_ x: [[Int]]?) { }
  func f2(_ x: [([Int]) -> [Int]]) { }
  func f2a(_ x: [([Int]?) -> [Int]?]?) { }
}

class Derived1 : Base1 {
  override func f0(_ x: Array<Int>) { }
  override func f0a(_ x: Optional<Array<Int>>) { }
  override func f1(_ x: Array<Array<Int>>) { }
  override func f1a(_ x: Optional<Array<Array<Int>>>) { }
  override func f2(_ x: Array<(Array<Int>) -> Array<Int>>) { }
  override func f2a(_ x: Optional<Array<(Optional<Array<Int>>) -> Optional<Array<Int>>>>) { }
}


// Array types in generic specializations.
struct X<T> { }

func testGenericSpec() {
  _ = X<[Int]>()
}

// Array types for construction.
func constructArray(_ n: Int) {
  var ones = [Int](repeating: 1, count: n)
  ones[5] = 0

  var matrix = [[Float]]()
  matrix[1][2] = 3.14159

  var _: [Int?] = [Int?]()
}

// Fix-Its from the old syntax to the new.

typealias FixIt0 = Int[] // expected-error{{array types are now written with the brackets around the element type}}{{20-20=[}}{{23-24=}}

// Make sure preCheckExpression() properly folds member types.

class Outer {
  class Middle {
    class Inner {}
    typealias Alias = Inner
  }

  typealias Alias = Middle
}

func takesInner(_: [Outer.Middle.Inner]) {}

takesInner([Outer.Middle.Inner]())
takesInner([Outer.Alias.Inner]())
takesInner([Outer.Middle.Alias]())
takesInner([Outer.Alias.Alias]())

takesInner([array.Outer.Middle.Inner]())
takesInner([array.Outer.Alias.Inner]())
takesInner([array.Outer.Middle.Alias]())
takesInner([array.Outer.Alias.Alias]())

// FIXME: We should support this with nested types of generic parameters also
protocol HasAssocType {
  associatedtype A
}

func takesAssocType<T : HasAssocType>(_: T, _: [T.A]) {}

func passAssocType<T : HasAssocType>(_ t: T) {
  takesAssocType(t, [T.A]())
  // expected-error@-1 {{cannot call value of non-function type '[T.A.Type]'}}
}
