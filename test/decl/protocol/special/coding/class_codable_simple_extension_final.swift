// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -swift-version 4

// Simple final classes where Codable conformance is added in extensions should
// be able to derive conformance for both Codable protocols.
final class SimpleClass {
  var x: Int = 1
  var y: Double = .pi
  static var z: String = "foo"

  func foo() {
    // They should receive a synthesized CodingKeys enum.
    let _ = SimpleClass.CodingKeys.self

    // The enum should have a case for each of the vars.
    let _ = SimpleClass.CodingKeys.x
    let _ = SimpleClass.CodingKeys.y

    // Static vars should not be part of the CodingKeys enum.
    let _ = SimpleClass.CodingKeys.z // expected-error {{type 'SimpleClass.CodingKeys' has no member 'z'}}
  }
}

extension SimpleClass : Codable {}

// They should receive synthesized init(from:) and an encode(to:).
let _ = SimpleClass.init(from:)
let _ = SimpleClass.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// struct.
let _ = SimpleClass.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
