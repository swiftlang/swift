// RUN: %target-typecheck-verify-swift

struct Ref<Value> { // expected-note {{'Value' declared as parameter to type 'Ref'}}
  static func foo(_ value: Int) {} // expected-note {{declared here}}
}

@dynamicMemberLookup
protocol RefConvertible {
  associatedtype Value

  var ref: Ref<Value> { get }

  subscript<T>(dynamicMember keyPath: WritableKeyPath<Value, T>) -> Ref<T> { get }
}

extension RefConvertible {
  public subscript<T>(dynamicMember keyPath: WritableKeyPath<Value, T>) -> Ref<T> {
    return .init()
  }
}

extension Ref : RefConvertible {
  var ref: Ref { return self }
}

func rdar_48994658() {
  Ref.foo() // expected-error {{missing argument for parameter #1 in call}}
  // expected-error@-1 {{generic parameter 'Value' could not be inferred}}
  // expected-note@-2 {{explicitly specify the generic arguments to fix this issue}} {{6-6=<Any>}}
}
