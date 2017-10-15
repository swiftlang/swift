// RUN: %target-typecheck-verify-swift -swift-version 3

func foo(fn: (() -> ()) -> ()) {
  fn { fn {} } // expected-warning {{passing a closure which captures a non-escaping function parameter 'fn' to a call to a non-escaping function parameter can allow re-entrant modification of a variable and will be illegal in Swift 4}}
}
