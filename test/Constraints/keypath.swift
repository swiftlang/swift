// RUN: %target-typecheck-verify-swift %S/Inputs/keypath.swift -primary-file %s

struct S {
  let i: Int

  init() {
    let _: WritableKeyPath<S, Int> = \.i // no error for Swift 3/4
  }
}

func test() {
  let _: WritableKeyPath<C, Int> = \.i // no error for Swift 3/4
}
