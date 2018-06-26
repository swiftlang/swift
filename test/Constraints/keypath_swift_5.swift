// RUN: %target-typecheck-verify-swift %S/Inputs/keypath.swift -primary-file %s -swift-version 5

struct S {
  let i: Int

  init() {
    let _: WritableKeyPath<S, Int> = \.i // expected-error {{type of expression is ambiguous without more context}}

    S()[keyPath: \S.i] = 1
    // expected-error@-1 {{cannot assign to immutable expression}}
  }
}

func test() {
  let _: WritableKeyPath<C, Int> = \.i // expected-error {{type of expression is ambiguous without more context}}

  C()[keyPath: \C.i] = 1
  // expected-error@-1 {{cannot assign to immutable expression}}

  let _ = C()[keyPath: \C.i] // no warning for a read
}
