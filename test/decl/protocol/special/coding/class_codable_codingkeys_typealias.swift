// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Simple classes with all Codable properties whose CodingKeys come from a
// typealias should get derived conformance to Codable.
class SimpleClass : Codable {
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
let _ = SimpleClass.init(from:)
let _ = SimpleClass.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// class.
let _ = SimpleClass.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
