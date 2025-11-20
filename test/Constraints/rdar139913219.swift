// RUN: %target-typecheck-verify-swift

// rdar://139913219 - Make sure we don't crash.

func bar(_ x: Int.Type, _: Int) {}
func bar<T>(_ x: T.Type, _: Int) {}

func foo() {
  bar(X<Int?>.self, .zero)
  // expected-error@-1 {{cannot find 'X' in scope}}
}
