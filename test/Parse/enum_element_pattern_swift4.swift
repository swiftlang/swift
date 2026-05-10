// RUN: %target-typecheck-verify-swift -swift-version 4

// https://github.com/apple/swift/issues/46040
// See test/Compatibility/enum_element_pattern.swift for Swift3 behavior.
// As for FIXME cases: see https://github.com/apple/swift/issues/46054

enum E {
  case A, B, C, D

  static func testE(e: E) {
    switch e {
    case A<UndefinedTy>(): // expected-error {{cannot find type 'UndefinedTy' in scope}}
    // expected-note@-1 {{while parsing this '<' as a type parameter bracket}}
    // expected-error@-2 {{cannot specialize non-generic type 'E'}}
    // expected-error@-3 {{enum case 'A' has no associated values}}
      break
    case B<Int>(): // expected-error {{cannot specialize non-generic type 'E'}}
    // expected-error@-1 {{enum case 'B' has no associated values}}
      break
    default:
      break;
    }
  }
}

func testE(e: E) {
  switch e {
  case E.A<UndefinedTy>(): // expected-error {{cannot find type 'UndefinedTy' in scope}}
  // expected-note@-1 {{while parsing this '<' as a type parameter bracket}}
  // expected-error@-2 {{cannot specialize non-generic type 'E'}}
  // expected-error@-3 {{enum case 'A' has no associated values}}
    break
  case E.B<Int>(): // expected-error {{cannot specialize non-generic type 'E'}}
  // expected-error@-1 {{enum case 'B' has no associated values}}
    break
  case .C(): // expected-error {{pattern with associated values does not match enum case 'C'}}
             // expected-note@-1 {{remove associated values to make the pattern match}} {{10-12=}} 
    break
  case .D(let payload): // expected-error{{pattern with associated values does not match enum case 'D'}}
                        // expected-note@-1 {{remove associated values to make the pattern match}} {{10-23=}}
    let _: () = payload
    break
  default:
    break
  }

  guard
    case .C() = e, // expected-error {{pattern with associated values does not match enum case 'C'}} 
                   // expected-note@-1 {{remove associated values to make the pattern match}} {{12-14=}}
    case .D(let payload) = e // expected-error {{pattern with associated values does not match enum case 'D'}}
                             // expected-note@-1 {{remove associated values to make the pattern match}} {{12-25=}}
  else { return }
}

extension E : Error {}
func canThrow() throws {
  throw E.A
}

do {
  try canThrow()
} catch E.A() { // expected-error {{pattern with associated values does not match enum case 'A'}}
                // expected-note@-1 {{remove associated values to make the pattern match}} {{12-14=}}
  // ..
} catch E.B(let payload) { // expected-error {{pattern with associated values does not match enum case 'B'}}
                           // expected-note@-1 {{remove associated values to make the pattern match}} {{12-25=}} 
  let _: () = payload
}
