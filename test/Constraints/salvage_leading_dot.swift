// RUN: %target-typecheck-verify-swift -solver-disable-crash-on-valid-salvage
// RUN: not --crash %target-swift-frontend -typecheck %s -solver-enable-crash-on-valid-salvage

struct S {
  static func foo(_ s: S?) -> S? {s}
  static func foo(_ s: S) -> S {s}
}

public func test() {
  let _: S? = .foo(S())
}

