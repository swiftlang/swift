// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -swift-version 4

final class Conditional<T> {
  var x: T
  var y: T?

  init() {
    fatalError()
  }
  func foo() {
    // They should receive a synthesized CodingKeys enum.
    let _ = Conditional.CodingKeys.self

    // The enum should have a case for each of the vars.
    let _ = Conditional.CodingKeys.x
    let _ = Conditional.CodingKeys.y

    // Static vars should not be part of the CodingKeys enum.
    let _ = Conditional.CodingKeys.z // expected-error {{type 'Conditional<T>.CodingKeys' has no member 'z'}}
  }
}

extension Conditional: Codable where T: Codable {}

// They should receive synthesized init(from:) and an encode(to:).
let _ = Conditional<Int>.init(from:)
let _ = Conditional<Int>.encode(to:)

// but only for Codable parameters.
struct Nonconforming {}
let _ = Conditional<Nonconforming>.init(from:) // expected-error {{type 'Nonconforming' does not conform to protocol 'Decodable'}}
let _ = Conditional<Nonconforming>.encode(to:) // expected-error {{type 'Nonconforming' does not conform to protocol 'Decodable'}}

// The synthesized CodingKeys type should not be accessible from outside the
// struct.
let _ = Conditional<Int>.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
