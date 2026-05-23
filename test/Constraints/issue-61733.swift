// RUN: %target-typecheck-verify-swift

// REQUIRES: asserts

// Issue: https://github.com/swiftlang/swift/issues/61733

protocol P {}

struct S: P {}

@_marker
protocol Marker {
}

func takesOptionalP(_: (some P)?) {}
// expected-note@-1 {{required by global function 'takesOptionalP' where 'some P' = 'any P'}}

func passOptional(foo: (any P)?) {
  // TODO(diagnostics): The message should suggest an optional unwrap here instead.
  takesOptionalP(foo) // expected-error {{type 'any P' cannot conform to 'P'}}
  // expected-note@-1 {{only concrete types such as structs, enums and classes can conform to protocols}}
}

struct NoOpening {
  var v: (any Marker)? = nil

  func takesOptional<T>(_: T?) {}

  func testMarkerWithOptionals() {
    takesOptional(self.v) // Ok
  }

  func testCompositions(v: (any P & Marker)?, t: (any P & Marker).Type) {
    func takesGeneric<T, U>(_: T, is: U.Type) {
    }

    takesGeneric(v, is: t) // Ok
  }
}
