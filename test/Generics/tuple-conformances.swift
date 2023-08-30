// RUN: %target-typecheck-verify-swift -enable-experimental-feature TupleConformances

// Because of -enable-experimental-feature TupleConformances
// REQUIRES: asserts

protocol P {
  associatedtype A
  associatedtype B

  func f()
}

extension () {}
// expected-error@-1 {{tuple extension must be written as extension of '(repeat each Element)'}}
// FIXME: Inaccurate

typealias Tuple<each Element> = (repeat each Element)

extension Tuple: P where repeat each Element: P {
  typealias A = (repeat (each Element).A)
  typealias B = Float
  func f() {}
}

extension Int: P {
  typealias A = Int
  typealias B = String
  func f() {}
}

func returnsPA<T: P>(_: T) -> T.A.Type {}
func returnsPB<T: P>(_: T) -> T.B.Type {}

func same<T>(_: T, _: T) {}

func useConformance() {
  same(returnsPA((1, 2, 3)), (Int, Int, Int).self)
  same(returnsPB((1, 2, 3)), Float.self)

  (1, 2, 3).f()
}

////

extension Tuple: Equatable where repeat each Element: Equatable {
  // FIXME: Hack
  @_disfavoredOverload
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    var result = true
    func update<E: Equatable>(lhs: E, rhs: E) {
      result = result && (lhs == rhs)
    }

    repeat update(lhs: each lhs, rhs: each rhs)
    return result
  }
}

extension Tuple: Hashable where repeat each Element: Hashable {
  public func hash(into hasher: inout Hasher) {
    repeat (each self).hash(into: &hasher)
  }
}
