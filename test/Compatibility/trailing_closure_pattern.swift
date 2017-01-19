// RUN: %target-typecheck-verify-swift -swift-version 3

// See test/stmts/statements.swift for Swift4 behavior.

enum Crasher28653 {
  struct S {
    static func ~=(pattern: () -> Int, val: S) -> Bool { return true }
  }
  case A(Int, S)
  case B(S)
  case C(S)
}

// Trailing closure as a pattern is allowed in Swift3.
func testEnumPatternWithTrailingClosure(e: Crasher28653) {
  typealias E = Crasher28653
  switch e {
    case .A(1) { 2 }: break // Ok.
    case .B() { 3 }: break// Ok.
    case .C { 4 }: break // Ok.
    case E.A(1) { 2 }: break // Ok.
    case E.B() { 3 }: break // Ok.
    case E.C { 4 }: break // Ok.
    default: break
  }
}
