// RUN: %target-typecheck-verify-swift -swift-version 3

// https://bugs.swift.org/browse/SR-3452
// See test/Parse/enum_element_pattern_swift4.swift for Swift4 behavior.

enum E {
  case A, B, C
}

func testE(e: E) {
  switch e {
  case E.A<UndefinedTy>(): // Ok.
    break
  case E.B<Int>(): // Ok.
    break
  case E.C(): // Ok.
    break
  default:
    break
  }
}
