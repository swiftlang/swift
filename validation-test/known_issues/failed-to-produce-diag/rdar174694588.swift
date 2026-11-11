// rdar://174694588
// RUN: %target-typecheck-verify-swift

@propertyWrapper
struct Wrapper<Value: Decodable>: Decodable {
  var wrappedValue: Value { fatalError() }
  init(transform: @Sendable @escaping (Int) throws -> Value) {}
}

struct Data: Decodable {
  init(capacity: Int) {}
}

struct Random {
  @Wrapper(transform: Data.init(capacity:)) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  var uuid: String
}
