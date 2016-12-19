// RUN: %target-typecheck-verify-swift -swift-version 4

// https://bugs.swift.org/browse/SR-3452
// See test/Compatibility/enum_element_pattern.swift for Swift3 behavior.

enum E {
  case A, B, C
}

func testE(e: E) {
  switch e {
  case E.A<UndefinedTy>(): // expected-error {{use of undeclared type 'UndefinedTy'}}
    break
  case E.B<Int>(): // expected-error {{cannot specialize a non-generic definition}} expected-note {{while parsing this '<' as a type parameter bracket}}
    break
  case E.C(): // Ok.
    break
  default:
    break
  }
}
