// rdar://147552222 (https://github.com/swiftlang/swift/issues/80189)
// RUN: %target-typecheck-verify-swift

protocol P {}

struct Foo: Identifiable {
  let id: Int
  let value: Int
}

var foo: Foo?
let foos: [Foo] = []

func test1() {
  foo = foos.first?.id // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func onAppear(_ action: () -> Void) {}

func test2() {
  onAppear { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    foo = foos.first?.id
  }
}

func test3() -> (any P)? {
  return foos.first?.id // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func takesString(_: String) {}

func test4() {
  takesString(foo?.value) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
