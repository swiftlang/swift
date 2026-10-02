// RUN: %target-typecheck-verify-swift

func foo() {
  var int {} // expected-error {{computed property must have an explicit type}} {{10-10=: <# Type #>}}
}

func bar() {
  var (a, b) {} // expected-error {{computed property must have an explicit type}} {{13-13=: <# Type #>}}
}
