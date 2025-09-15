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

// Make sure preCheckTarget() properly folds member types.

class Outer {
  class Middle {
    class Inner {}
    class GenericInner<V> {}

    typealias Alias = Inner
  }

  class GenericMiddle<U> {
    class Inner {}
  }

  typealias Alias = Middle
}

class GenericOuter<T> {
  class Middle {
    class Inner {}
  }
}

func takesMiddle(_: [Outer.Middle]) {}

takesMiddle([Outer.Middle]())

func takesInner(_: [Outer.Middle.Inner]) {}

takesInner([Outer.Middle.Inner]())
takesInner([Outer.Alias.Inner]())
takesInner([Outer.Middle.Alias]())
takesInner([Outer.Alias.Alias]())

takesInner([array.Outer.Middle.Inner]())
takesInner([array.Outer.Alias.Inner]())
takesInner([array.Outer.Middle.Alias]())
takesInner([array.Outer.Alias.Alias]())

func takesMiddle(_: [GenericOuter<Int>.Middle]) {}

takesMiddle([GenericOuter<Int>.Middle]())

func takesInner(_: [GenericOuter<Int>.Middle.Inner]) {}

takesInner([GenericOuter<Int>.Middle.Inner]())
takesInner([array.GenericOuter<Int>.Middle.Inner]())

func takesMiddle(_: [Outer.GenericMiddle<Int>]) {}

takesMiddle([Outer.GenericMiddle<Int>]())

func takesInner(_: [Outer.GenericMiddle<Int>.Inner]) {}

takesInner([Outer.GenericMiddle<Int>.Inner]())
takesInner([array.Outer.GenericMiddle<Int>.Inner]())

func takesInner(_: [Outer.Middle.GenericInner<Int>]) {}

takesInner([Outer.Middle.GenericInner<Int>]())
takesInner([array.Outer.Middle.GenericInner<Int>]())

protocol HasAssocType {
  associatedtype A
}

func takesAssocType<T : HasAssocType>(_: T, _: [T.A], _: [T.A?]) {}

func passAssocType<T : HasAssocType>(_ t: T) {
  takesAssocType(t, [T.A](), [T.A?]())
}

// https://github.com/apple/swift/issues/53530
do {
  let _ = [[1, 2, 3][0]] // ok
  let _ = [[1, 2, 3] [1]] // expected-warning {{unexpected subscript in array literal; did you mean to write two separate elements instead?}}
  // expected-note@-1 {{add a separator between the elements}}{{21-21=,}}
  // expected-note@-2 {{remove the space between the elements to silence this warning}}{{21-22=}}
  let _ = [
    [1, 2, 3] [1] // expected-warning {{unexpected subscript in array literal; did you mean to write two separate elements instead?}}
  // expected-note@-1 {{add a separator between the elements}}{{14-14=,}}
  // expected-note@-2 {{remove the space between the elements to silence this warning}}{{14-15=}}
  ]
}

struct TupleWith<T> {
  typealias And<U> = (T, U)
}
_ = [TupleWith<String>.And<Int>](repeating: ("", 0), count: 0)
_ = [TupleWith.And<Int>](repeating: ("", 0), count: 0)
_ = [TupleWith<String>.And](repeating: ("", 0), count: 0)
_ = [TupleWith.And](repeating: ("", 0), count: 0)
