// RUN: %target-typecheck-verify-swift -swift-version 6

struct S<T> {}
extension S: Sendable where T: Sendable {}
extension S where T == Int {
  func foo() {}
  func foo(x: String) {}
}

// Make sure we can type-check this.
let _: () -> Void = S().foo
let _: (String) -> Void = S().foo
