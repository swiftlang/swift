// RUN: %target-typecheck-verify-swift

protocol P {}
struct S<V> where V: P { // expected-note {{where 'V' = 'Double'}}
  var value: V
}

struct A {
  subscript<T>(_ v: S<T>) -> A { // expected-note {{where 'T' = 'Double'}}
    fatalError()
  }
}

func foo(_ v: Double) {
  _ = A()[S(value: v)]
// expected-error@-1 {{subscript 'subscript(_:)' requires that 'Double' conform to 'P'}}
// expected-error@-2 {{referencing initializer 'init(value:)' on 'S' requires that 'Double' conform to 'P'}}
}
