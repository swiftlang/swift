// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

struct NonCodable {}

// Enums which have a default parameter that is not Codable, but which has a
// default value and is skipped in its CodingKeys should still derive
// conformance.
enum SimpleEnum : Codable {
  case x(
      w: NonCodable = NonCodable(),
      x: Int,
      y: Double)

  private enum XCodingKeys : String, CodingKey { // expected-note {{'XCodingKeys' declared here}}
    case x
    case y
  }
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = SimpleEnum.init(from:)
let _ = SimpleEnum.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// struct.
let _ = SimpleEnum.XCodingKeys.self // expected-error {{'XCodingKeys' is inaccessible due to 'private' protection level}}
