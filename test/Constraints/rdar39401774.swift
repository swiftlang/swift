// RUN: %target-typecheck-verify-swift

class A {
  var foo: Int? { return 42 } // expected-note {{found this candidate}}
}

protocol P1 {
  var foo: Int? { get } // expected-note {{found this candidate}}
}

protocol P2 : P1 {
  var bar: Int? { get }
}

extension P2 where Self: A {
  var bar: Int? {
    guard let foo = foo else { return 0 } // expected-error {{ambiguous use of 'foo'}}
    return foo
  }
}
