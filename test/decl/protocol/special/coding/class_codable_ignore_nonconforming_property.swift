// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

struct NonCodable {}

// Classes which have a default property that is not Codable, but which has a
// default value and is skipped in its CodingKeys should still derive
// conformance.
class SimpleClass : Codable {
  var w: NonCodable = NonCodable()
  var x: Int = 1
  var y: Double = .pi
  static var z: String = "foo"

  private enum CodingKeys : String, CodingKey { // expected-note {{'CodingKeys' declared here}}
    case x
    case y
  }
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = SimpleClass.init(from:)
let _ = SimpleClass.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// class.
let _ = SimpleClass.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
