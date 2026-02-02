// RUN: %target-typecheck-verify-swift

struct Foo<T, U> { // expected-note {{incorrect labels for candidate (have: '(_:)', expected: '(value:)')}}
  var value: U
  func bar() -> Foo<T, U> {
    return Foo(value)
    // expected-error@-1 {{no exact matches in call to initializer}}
  }
}

extension Foo where T == U { // expected-note {{candidate requires that the types 'T' and 'U' be equivalent (requirement specified as 'T' == 'U')}}
  init(_ value: U)  {
    self.value = value
  }
}
