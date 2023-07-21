// RUN: %target-typecheck-verify-swift -enable-experimental-feature TupleConformances -parse-stdlib

// REQUIRES: asserts

import Swift

protocol P {
  associatedtype A
  associatedtype B

  func f()
}

extension Builtin.TheTupleType: P where repeat each Elements: P {
  typealias A = (repeat (each Elements).A)
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
