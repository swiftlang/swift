// RUN: %target-typecheck-verify-swift %S/Inputs/keypath.swift -primary-file %s -swift-version 5

struct S {
  let i: Int

  init() {
    let _: WritableKeyPath<S, Int> = \.i // expected-error {{type of expression is ambiguous without more context}}
  }
}

func test() {
  let _: WritableKeyPath<C, Int> = \.i // expected-error {{type of expression is ambiguous without more context}}
}
