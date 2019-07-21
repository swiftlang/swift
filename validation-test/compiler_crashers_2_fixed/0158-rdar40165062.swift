// RUN: %target-typecheck-verify-swift

struct Foo<T, U> { // expected-note {{'U' declared as parameter to type 'Foo'}}
  var value: U
  func bar() -> Foo<T, U> {
    return Foo(value)
    // expected-error@-1 {{generic parameter 'U' could not be inferred}}
    // expected-note@-2 {{explicitly specify the generic arguments to fix this issue}}
  }
}

extension Foo where T == U {
  init(_ value: U)  {
    self.value = value
  }
}
