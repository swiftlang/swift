// RUN: %target-typecheck-verify-swift

// SR-13992

protocol V {}

protocol P1 {}
protocol P2 {
  func bar() -> V
}
protocol P3 {}

struct S<T> {
  var foo: T
}

extension S : P1 {}
extension S : P2 where T: P3 { // expected-note {{requirement from conditional conformance of 'S<any V>' to 'P2'}}
  func bar() -> V { fatalError() }
}

struct Q {
  var foo: V

  func test() -> P1 & P2 {
    S(foo: foo) // expected-error {{type 'any V' cannot conform to 'P3'}}
    // expected-note@-1 {{only concrete types such as structs, enums and classes can conform to protocols}}
  }
}
