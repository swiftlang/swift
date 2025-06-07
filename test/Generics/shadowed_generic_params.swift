// RUN: %target-typecheck-verify-swift

struct Outer<T, U> { // expected-note 2{{'T' previously declared here}}
  struct Inner<V> { // expected-note 2{{'V' previously declared here}}
    func foo<T>(_: T) {} // expected-warning {{generic parameter 'T' shadows generic parameter from outer scope with the same name; this is an error in the Swift 6 language mode}}
    func bar<V>(_: V) {} // expected-warning {{generic parameter 'V' shadows generic parameter from outer scope with the same name; this is an error in the Swift 6 language mode}}
  }
}

extension Outer.Inner {
  func baz<T>(_: T) {} // expected-warning {{generic parameter 'T' shadows generic parameter from outer scope with the same name; this is an error in the Swift 6 language mode}}
  func quux<V>(_: V) {} // expected-warning {{generic parameter 'V' shadows generic parameter from outer scope with the same name; this is an error in the Swift 6 language mode}}
}

extension Sequence {
  func bing<Self>(_: Self) {} // expected-warning {{generic parameter 'Self' shadows generic parameter from outer scope with the same name; this is an error in the Swift 6 language mode}}
}
