// RUN: %target-typecheck-verify-swift

class A<T> {
  var foo: Int? { return 42 }      // expected-note {{found this candidate}}
  func baz() -> T { fatalError() } // expected-note {{found this candidate}}
  func fiz() -> Int { return 42 }  // expected-note {{found this candidate}}
}

protocol P1 {
  associatedtype T
  var foo: Int? { get } // expected-note {{found this candidate}}
  func baz() -> T       // expected-note {{found this candidate}}
  func fiz() -> Int     // expected-note {{found this candidate}}
}

protocol P2 : P1 {
  var bar: Int? { get }
}

extension P2 where Self: A<Int> {
  var bar: Int? {
    guard let foo = foo else { return 0 } // expected-error {{ambiguous use of 'foo'}}
    let _ = baz() // expected-error {{ambiguous use of 'baz()'}}
    return fiz()  // expected-error {{ambiguous use of 'fiz()'}}
  }
}
