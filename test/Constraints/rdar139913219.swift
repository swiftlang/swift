// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// rdar://139913219 - Make sure we don't crash.

func bar(_ x: Int.Type, _: Int) {}
func bar<T>(_ x: T.Type, _: Int) {}

func foo() {
  // FIXME: We shouldn't be failing to produce a diagnostic.
  // Once resolved, remove '-verify-ignore-unknown'
  bar(X<Int?>.self, .zero)
  // expected-error@-1 {{cannot find 'X' in scope}}
  // expected-error@-2 {{failed to produce diagnostic for expression}}
}
