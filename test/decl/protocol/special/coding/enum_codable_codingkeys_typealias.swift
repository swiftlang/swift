// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Simple enums with all Codable parameters whose CodingKeys come from a
// typealias should get derived conformance to Codable.
enum SimpleEnum : Codable {
  case x(Int)
  case y(String)

  private typealias CodingKeys = A // expected-note {{'CodingKeys' declared here}}
  private typealias A = B
  private typealias B = C
  private typealias C = MyRealCodingKeys

  private enum MyRealCodingKeys : String, CodingKey {
    case x
    case y
  }
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = SimpleEnum.init(from:)
let _ = SimpleEnum.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// enum.
let _ = SimpleEnum.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}

// Enums with CodingKeys which are typealiases that don't point to a valid
// nominal type should produce errors.
struct EnumWithUndeclaredCodingKeys : Codable { // expected-error {{type 'EnumWithUndeclaredCodingKeys' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'EnumWithUndeclaredCodingKeys' does not conform to protocol 'Encodable'}}
  private typealias CodingKeys = NonExistentType // expected-error {{cannot find type 'NonExistentType' in scope}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'CodingKeys' does not conform to CodingKey}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' does not conform to CodingKey}}
}
