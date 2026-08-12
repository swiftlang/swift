// RUN: %target-typecheck-verify-swift -solver-disable-diagnose-valid-salvage
// RUN: %target-typecheck-verify-swift -solver-enable-diagnose-valid-salvage -verify-additional-prefix salvage-
// RUN: not --crash %target-swift-frontend -typecheck %s -solver-enable-crash-fail-diagnostic

// Once this bug is fixed, please add a new test to exercise -solver-enable-crash-fail-diagnostic.

struct S {
  static func foo(_ s: S?) -> S? {s}
  static func foo(_ s: S) -> S {s}
}

public func test() {
  let _: S? = .foo(S())
  // expected-salvage-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}

