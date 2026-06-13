// RUN: %target-typecheck-verify-swift -module-name main -language-mode 5 -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -module-name main -language-mode 6 -verify-additional-prefix swift6-

struct S {
  func foo(c: S) -> S {}
  func bar(_ c: S) -> S {}

  // The different diagnostics here are due to the inference of @Sendable when attempting to reference the decl on the type rather than instance.
  let x1: ((S) -> S).Type = type(of: foo)
  // expected-swift5-error@-1 {{cannot convert value of type '((S) -> (S) -> S).Type' to specified type '((S) -> S).Type'}}
  // expected-swift6-error@-2 {{cannot use instance member 'foo' within property initializer; property initializers run before 'self' is available}}

  let x2: ((S) -> S).Type = type(of: bar)
  // expected-error@-1 {{cannot use instance member 'bar' within property initializer; property initializers run before 'self' is available}}

  // FIXME: https://github.com/swiftlang/swift/issues/89920
  let y1: (S) -> S = foo
  // expected-error@-1 {{cannot convert value of type '(main.S) -> main.S' to specified type '(main.S) -> main.S'}}
  // expected-error@-2 {{cannot use instance member 'foo' within property initializer; property initializers run before 'self' is available}}

  let y2: (S) -> S = bar
  // expected-error@-1 {{cannot use instance member 'bar' within property initializer; property initializers run before 'self' is available}}
}
