// RUN: %target-typecheck-verify-swift

protocol P {}

struct S<T> {}

class A : P {}

extension S where T : P {
  typealias A = Int
  typealias B<U> = S<U>
}

_ = S<A>.A()           // Ok
_ = S<Int>.A()         // expected-error {{type 'Int' does not conform to protocol 'P'}}
_ = S<String>.B<Int>() // expected-error {{type 'String' does not conform to protocol 'P'}}

func foo<T>(_ s: S<T>.Type) {
  _ = s.A() // expected-error {{type 'T' does not conform to protocol 'P'}}
}

func bar<T: P>(_ s: S<T>.Type) {
  _ = s.A() // Ok
}
