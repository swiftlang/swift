// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/keypath.swift -primary-file %s -swift-version 5

struct S {
  let i: Int

  init() {
    let _: WritableKeyPath<S, Int> = \.i // expected-error {{cannot convert value of type 'KeyPath<S, Int>' to specified type 'WritableKeyPath<S, Int>'}}

    S()[keyPath: \.i] = 1
    // expected-error@-1 {{cannot assign through subscript: immutable key path}}
  }
}

func test() {
  let _: WritableKeyPath<C, Int> = \.i // expected-error {{cannot convert value of type 'KeyPath<C, Int>' to specified type 'WritableKeyPath<C, Int>'}}

  C()[keyPath: \.i] = 1
  // expected-error@-1 {{cannot assign through subscript: immutable key path}}

  let _ = C()[keyPath: \.i] // no warning for a read
}
