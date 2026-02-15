// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/49778

func statementStartingWithOpenBrace() {
  { print("hello") } // expected-error {{statement beginning with '{' is interpreted as a closure expression}} expected-note {{did you mean to use a 'do' statement?}} {{3-3=do }}
}

func explicitClosureExprStillUsesGeneralDiagnostic() {
  { _ in print("hello") } // expected-error {{closure expression is unused}}
}
