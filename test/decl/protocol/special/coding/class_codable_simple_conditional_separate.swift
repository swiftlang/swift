// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -swift-version 4

class Conditional<T> {
  var x: T
  var y: T?

  init() {
  // expected-note@-1 2 {{did you mean 'init'}}
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

extension Conditional: Encodable where T: Encodable { // expected-note {{where 'T' = 'OnlyDec'}}
}

extension Conditional: Decodable where T: Decodable {
    // expected-error@-1 2 {{implementation of 'Decodable' for non-final class cannot be automatically synthesized in extension because initializer requirement 'init(from:)' can only be be satisfied by a 'required' initializer in the class definition}}
}

struct OnlyEnc: Encodable {}
struct OnlyDec: Decodable {}

// They should receive synthesized init(from:) and an encode(to:).
let _ = Conditional<OnlyDec>.init(from:) // expected-error {{type 'Conditional<OnlyDec>' has no member 'init(from:)'}}
let _ = Conditional<OnlyEnc>.encode(to:)

// but only for the appropriately *codable parameters.
let _ = Conditional<OnlyEnc>.init(from:) // expected-error {{type 'Conditional<OnlyEnc>' has no member 'init(from:)'}}
let _ = Conditional<OnlyDec>.encode(to:) // expected-error {{referencing instance method 'encode(to:)' on 'Conditional' requires that 'OnlyDec' conform to 'Encodable'}}

// The synthesized CodingKeys type should not be accessible from outside the
// struct.
let _ = Conditional<Int>.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
