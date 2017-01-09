// RUN: %target-typecheck-verify-swift -swift-version 3

// https://bugs.swift.org/browse/SR-3452
// See test/Parse/enum_element_pattern_swift4.swift for Swift4 behavior.

enum E {
  case A, B, C, D

  static func testE(e: E) {
    switch e {
    case A<UndefinedTy>(): // expected-warning {{cannot specialize enum case; ignoring generic argument, which will be rejected in future version of Swift}} {{11-24=}}
      break
    case B<Int>(): // expected-warning {{cannot specialize enum case; ignoring generic argument, which will be rejected in future version of Swift}} {{11-16=}}
      break
    default:
      break;
    }
  }
}

func testE(e: E) {
  switch e {
  case E.A<UndefinedTy>(): // expected-warning {{cannot specialize enum case; ignoring generic argument, which will be rejected in future version of Swift}} {{11-24=}}
    break
  case E.B<Int>(): // expected-warning {{cannot specialize enum case; ignoring generic argument, which will be rejected in future version of Swift}} {{11-16=}}
    break
  case .C(): // Ok.
    break
  case .D(let payload): // Ok. 'payload' has type '()'.
    let _: () = payload
    break
  default:
    break
  }

  guard
    case .C() = e, // Ok. SILGen assert this, but no-assert Swift3 GM build didn't assert.
    case .D(let payload) = e // FIXME: Should be rejected. Swift3 IRGen verifier did catch this.
  else { return }
  print(payload)
}

extension E : Error {}
func canThrow() throws {
  throw E.A
}

do {
  try canThrow()
} catch E.A() { // Ok.
  // ..
} catch E.B(let payload) { // Ok. 'payload' has type '()'.
  let _: () = payload
}
