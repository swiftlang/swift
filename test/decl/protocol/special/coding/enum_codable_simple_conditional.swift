// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -swift-version 4

enum Conditional<T> {
  case a(x: T, y: T?)
  case b(z: [T])

  func foo() {
    // They should receive a synthesized CodingKeys enum.
    let _ = Conditional.CodingKeys.self
    let _ = Conditional.ACodingKeys.self

    // The enum should have a case for each of the cases.
    let _ = Conditional.CodingKeys.a
    let _ = Conditional.CodingKeys.b

    // The enum should have a case for each of the parameters.
    let _ = Conditional.ACodingKeys.x
    let _ = Conditional.ACodingKeys.y

    let _ = Conditional.BCodingKeys.z
  }
}

extension Conditional: Codable where T: Codable { // expected-note 4{{where 'T' = 'Nonconforming'}}
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = Conditional<Int>.init(from:)
let _ = Conditional<Int>.encode(to:)

// but only for Codable parameters.
struct Nonconforming {}
let _ = Conditional<Nonconforming>.init(from:) // expected-error {{referencing initializer 'init(from:)' on 'Conditional' requires that 'Nonconforming' conform to 'Encodable'}}
// expected-error@-1 {{referencing initializer 'init(from:)' on 'Conditional' requires that 'Nonconforming' conform to 'Decodable'}}
let _ = Conditional<Nonconforming>.encode(to:) // expected-error {{referencing instance method 'encode(to:)' on 'Conditional' requires that 'Nonconforming' conform to 'Encodable'}}
// expected-error@-1 {{referencing instance method 'encode(to:)' on 'Conditional' requires that 'Nonconforming' conform to 'Decodable'}}

// The synthesized CodingKeys type should not be accessible from outside the
// enum.
let _ = Conditional<Int>.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
let _ = Conditional<Int>.ACodingKeys.self // expected-error {{'ACodingKeys' is inaccessible due to 'private' protection level}}
let _ = Conditional<Int>.BCodingKeys.self // expected-error {{'BCodingKeys' is inaccessible due to 'private' protection level}}
