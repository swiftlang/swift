// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/keypath.swift -primary-file %s -swift-version 5

struct S {
  let i: Int

  init() {
    let _: WritableKeyPath<S, Int> = \.i // expected-error {{cannot convert key path type 'KeyPath<S, Int>' to contextual type 'WritableKeyPath<S, Int>'}}

    S()[keyPath: \.i] = 1
    // expected-error@-1 {{cannot assign through subscript: key path is read-only}}
  }
}

func test() {
  let _: WritableKeyPath<C, Int> = \.i // expected-error {{cannot convert key path type 'KeyPath<C, Int>' to contextual type 'WritableKeyPath<C, Int>'}}

  C()[keyPath: \.i] = 1
  // expected-error@-1 {{cannot assign through subscript: key path is read-only}}

  let _ = C()[keyPath: \.i] // no warning for a read
}


struct T {
  private(set) var a: Int
  init(a: Int) {
    self.a = a
  }
}

func testReadOnlyKeyPathDiagnostics() {
  let path = \T.a
  var t = T(a: 3)
  t[keyPath: path] = 4 // expected-error {{cannot assign through subscript: 'path' is a read-only key path}}
  t[keyPath: \T.a] = 4 // expected-error {{cannot assign through subscript: key path is read-only}}
}
