// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Simple structs with all Codable properties whose CodingKeys come from a
// typealias should get derived conformance to Codable.
struct SimpleStruct : Codable {
  var x: Int = 1
  var y: Double = .pi
  static var z: String = "foo"

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
let _ = SimpleStruct.init(from:)
let _ = SimpleStruct.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// struct.
let _ = SimpleStruct.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
