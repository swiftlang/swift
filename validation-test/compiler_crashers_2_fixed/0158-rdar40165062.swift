// RUN: %target-typecheck-verify-swift

struct Foo<T, U> {
  var value: U
  func bar() -> Foo<T, U> {
    return Foo(value)
    // expected-error@-1 {{referencing initializer 'init(_:)' on 'Foo' requires the types 'T' and 'U' be equivalent}}
  }
}

extension Foo where T == U { // expected-note {{where 'T' = 'T', 'U' = 'U'}}
  init(_ value: U)  {
    self.value = value
  }
}
