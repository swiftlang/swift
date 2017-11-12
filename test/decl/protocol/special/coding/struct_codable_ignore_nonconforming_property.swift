// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

struct NonCodable {}

// Structs which have a default property that is not Codable, but which has a
// default value and is skipped in its CodingKeys should still derive
// conformance.
struct SimpleStruct : Codable {
  var w: NonCodable = NonCodable()
  var x: Int
  var y: Double
  static var z: String = "foo"

  private enum CodingKeys : String, CodingKey { // expected-note {{'CodingKeys' declared here}}
    case x
    case y
  }
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = SimpleStruct.init(from:)
let _ = SimpleStruct.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// struct.
let _ = SimpleStruct.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
