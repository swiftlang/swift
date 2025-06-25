// RUN: %target-typecheck-verify-swift

// Make sure we don't run into a request cycle for the below use of 'CodingKeys'
// in an `init(from:)` signature.

protocol P {
  associatedtype X
}

struct S: Codable {
  var foo: String?

  enum CodingKeys: CodingKey {
    case foo
  }

  init<T: P>(from: T) where T.X == CodingKeys {}
}
