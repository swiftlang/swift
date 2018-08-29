// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/keypath.swift -primary-file %s -swift-version 5

struct S {
  let i: Int

  init() {
    let _: WritableKeyPath<S, Int> = \.i // expected-error {{type of expression is ambiguous without more context}}

    S()[keyPath: \.i] = 1
    // expected-error@-1 {{cannot assign through subscript: immutable key path}}
  }
}

func test() {
  let _: WritableKeyPath<C, Int> = \.i // expected-error {{type of expression is ambiguous without more context}}

  C()[keyPath: \.i] = 1
  // expected-error@-1 {{cannot assign through subscript: immutable key path}}

  let _ = C()[keyPath: \.i] // no warning for a read
}
