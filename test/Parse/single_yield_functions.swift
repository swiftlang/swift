// RUN: %target-typecheck-verify-swift -enable-experimental-feature SingleYieldFunctions -disable-experimental-parser-round-trip
// RUN: not %target-swift-frontend -typecheck %s -disable-experimental-parser-round-trip 2>&1 | %FileCheck %s --check-prefix=NOFLAG

// REQUIRES: swift_feature_SingleYieldFunctions

// NOFLAG: error: '-> yield' requires '-enable-experimental-feature SingleYieldFunctions'

class Klass {}

struct Wrapper {
  var k = Klass()

  func owned() -> yield Klass { yield Klass() }
  func borrowed() -> yield borrow Klass { yield k }
  mutating func mutable() -> yield inout Klass { yield &k }
  static func staticOwned() -> yield Klass { yield Klass() }

  mutating func needsAmpersand() -> yield inout Klass {
    yield k // expected-error {{yielding mutable value of type 'Klass' requires explicit '&'}}
  }
}

func globalOwned() -> yield Klass { yield Klass() }

func outer() {
  func localBorrowed() -> yield borrow Klass { yield Klass() }
}

// `yield` is still a type name when it isn't followed by a type.
struct yield {}
func returnsYieldType() -> yield { return yield() }

struct Invalid {
  init() -> yield Klass {} // expected-error {{initializers cannot have a result type}}
  static func +(a: Invalid, b: Invalid) -> yield Klass { fatalError() } // expected-error {{operator functions cannot be single-yield functions}}
}

struct E: Error {}

func throwing() throws -> yield Klass { yield Klass() }
func typedThrowing() throws(E) -> yield Klass { yield Klass() }
func rethrowing(_ f: () throws -> Void) rethrows -> yield Klass { yield Klass() }
func asyncYield() async -> yield Klass { yield Klass() } // expected-error {{single-yield functions cannot be 'async'}}
func asyncThrowing() async throws -> yield Klass { yield Klass() } // expected-error {{single-yield functions cannot be 'async'}}

func takesFunction(_ f: () -> yield Klass) {} // expected-error {{single-yield function types are not yet supported}}
