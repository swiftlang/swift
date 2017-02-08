// RUN: %target-typecheck-verify-swift -swift-version 4

// https://bugs.swift.org/browse/SR-3452
// See test/Compatibility/enum_element_pattern.swift for Swift3 behavior.
// As for FIXME cases: see https://bugs.swift.org/browse/SR-3466

enum E {
  case A, B, C, D

  static func testE(e: E) {
    switch e {
    case A<UndefinedTy>(): // expected-error {{use of undeclared type 'UndefinedTy'}}
      break
    case B<Int>(): // expected-error {{cannot specialize a non-generic definition}} expected-note {{while parsing this '<' as a type parameter bracket}}
      break
    default:
      break;
    }
  }
}

func testE(e: E) {
  switch e {
  case E.A<UndefinedTy>(): // expected-error {{use of undeclared type 'UndefinedTy'}}
    break
  case E.B<Int>(): // expected-error {{cannot specialize a non-generic definition}} expected-note {{while parsing this '<' as a type parameter bracket}}
    break
  case .C(): // FIXME: This should be rejected as well.
    break
  case .D(let payload): // FIXME: ditto.
    let _: () = payload
    break
  default:
    break
  }

  guard
    // Currently, these will be asserted in SILGen,
    // or in no-assert build, verify-failed in IRGen
    case .C() = e, // FIXME: Should be rejected.
    case .D(let payload) = e // FIXME: ditto.
  else { return }
}

extension E : Error {}
func canThrow() throws {
  throw E.A
}

do {
  try canThrow()
} catch E.A() { // FIXME: Should be rejected.
  // ..
} catch E.B(let payload) { // FIXME: ditto.
  let _: () = payload
}
