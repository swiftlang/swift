// RUN: %target-typecheck-verify-swift

struct Foo<T, U> {
  var value: U
  func bar() -> Foo<T, U> {
    return Foo(value) // expected-error {{generic parameter 'T' could not be inferred}}
  }
}

extension Foo where T == U {
  init(_ value: U)  {
    self.value = value
  }
}
