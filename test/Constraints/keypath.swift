// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/keypath.swift -primary-file %s

struct S {
  let i: Int

  init() {
    let _: WritableKeyPath<S, Int> = \.i // no error for Swift 3/4

    S()[keyPath: \.i] = 1
    // expected-error@-1 {{cannot assign to immutable expression}}
  }
}

func test() {
  let _: WritableKeyPath<C, Int> = \.i // no error for Swift 3/4

  C()[keyPath: \.i] = 1   // warning on write with literal keypath
  // expected-warning@-1 {{forming a writable keypath to property}}

  let _ = C()[keyPath: \.i] // no warning for a read
}
