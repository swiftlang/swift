// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -swift-version 6
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -swift-version 5
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple -swift-version 5 -enable-upcoming-feature InferSendableFromCaptures

// https://github.com/swiftlang/swift/issues/92911
// An identity key path unifies its root and value type variables. The root
// can be bound to a tuple containing an unresolved pack expansion before it
// is matched against the contextual value type.
struct FormActionPost<each Value> {
  func get<T>(
    _ keyPath: KeyPath<(repeat Result<each Value, any Error>), Result<T, any Error>>,
    _ name: String
  ) throws -> T {
    fatalError()
  }
}

struct Post {}

func process(_ post: FormActionPost<Post>) throws -> Post {
  try post.get(\.self, "fields")
}

func processGeneric<T>(_ post: FormActionPost<T>) throws -> T {
  try post.get(\.self, "fields")
}

struct Values<each Element> {
  func get<T>(_ path: KeyPath<(repeat each Element), T>) -> T {
    fatalError()
  }

  func identity(_ path: KeyPath<(repeat each Element), (repeat each Element)>) {}
}

func scalarIdentity(_ values: Values<Int>) -> Int {
  values.get(\.self)
}

func genericIdentity<T>(_ values: Values<T>) -> T {
  values.get(\.self)
}

func tupleIdentity(_ values: Values<Int, String>) -> (Int, String) {
  values.get(\.self)
}

func emptyIdentity(_ values: Values<>) -> () {
  values.get(\.self)
}

func unresolvedIdentity<each T>(_ values: Values<repeat each T>) {
  values.identity(\.self)
}

func invalidScalarIdentity(_ values: Values<Int>) -> String {
  values.get(\.self) // expected-error {{cannot convert return expression of type 'Int' to return type 'String'}}
}

func invalidTupleIdentity(_ values: Values<Int, String>) -> Int {
  values.get(\.self) // expected-error {{cannot convert return expression of type '(Int, String)' to return type 'Int'}}
}
