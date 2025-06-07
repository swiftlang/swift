// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/75389

@available(*, unavailable)
func unavailableFn() -> Int { 0 }
// expected-note@-1 5{{'unavailableFn()' has been explicitly marked unavailable here}}

if case unavailableFn() = 0 {}
// expected-error@-1 {{'unavailableFn()' is unavailable}}

switch Bool.random() {
case true where unavailableFn() == 1:
  // expected-error@-1 {{'unavailableFn()' is unavailable}}
  break
default:
  break
}
switch 0 {
case unavailableFn():
  // expected-error@-1 {{'unavailableFn()' is unavailable}}
  break
default:
  break
}
let _ = {
  switch Bool.random() {
  case true where unavailableFn() == 1:
    // expected-error@-1 {{'unavailableFn()' is unavailable}}
    break
  default:
    break
  }
  switch 0 {
  case unavailableFn():
    // expected-error@-1 {{'unavailableFn()' is unavailable}}
    break
  default:
    break
  }
}

struct S {}

func ~= (lhs: S.Type, rhs: S.Type) -> Bool { true }

if case (S) = S.self {}
// expected-error@-1 {{expected member name or initializer call after type name}}
// expected-note@-2 {{add arguments after the type to construct a value of the type}}
// expected-note@-3 {{use '.self' to reference the type object}}

if case ({ if case (S) = S.self { true } else { false } }()) = true {}
// expected-error@-1 {{expected member name or initializer call after type name}}
// expected-note@-2 {{add arguments after the type to construct a value of the type}}
// expected-note@-3 {{use '.self' to reference the type object}}
