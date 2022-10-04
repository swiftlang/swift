// RUN: %target-typecheck-verify-swift

protocol P {}

protocol Key {
  associatedtype A: P
  // expected-note@-1 {{unable to infer associated type 'A' for protocol 'Key'}}
  static var value: A { get }
}

struct Values {
  subscript<K: Key>(type: K.Type) -> K.A {
    fatalError()
  }
}

enum MyKey: Key { // expected-error {{type 'MyKey' does not conform to protocol 'Key'}}
  static let value = 1
  // expected-note@-1 {{candidate would match and infer 'A' = 'Int' if 'Int' conformed to 'P'}}
}

extension Values {
  var myValue: Int {
    get { self[MyKey.self] }
  }
}
