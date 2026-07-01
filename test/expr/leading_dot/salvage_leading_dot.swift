// RUN: %target-typecheck-verify-swift -solver-disable-crash-on-valid-salvage
// RUN: %target-swift-frontend -typecheck %s -solver-enable-crash-on-valid-salvage -verify

struct S {
  static func foo(_ s: S?) -> S? {s} // expected-note {{found this candidate}}
  static func foo(_ s: S) -> S {s}   // expected-note {{found this candidate}}
}

public func test() {
  // Optional injection happens either at the argument or result of the chain.
  let _: S? = .foo(S()) // expected-error {{ambiguous use of 'foo'}}
}

