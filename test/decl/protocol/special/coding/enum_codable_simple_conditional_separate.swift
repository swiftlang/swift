// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -swift-version 4

enum Conditional<T> {
  case a(x: T, y: T?)
  case b(z: [T])

  func foo() {
    // They should receive a synthesized CodingKeys enum.
    let _ = Conditional.CodingKeys.self
    let _ = Conditional.ACodingKeys.self
    let _ = Conditional.BCodingKeys.self

    // The enum should have a case for each of the cases.
    let _ = Conditional.CodingKeys.a
    let _ = Conditional.CodingKeys.b

    // The enum should have a case for each of the vars.
    let _ = Conditional.ACodingKeys.x
    let _ = Conditional.ACodingKeys.y

    let _ = Conditional.BCodingKeys.z
  }
}

extension Conditional: Encodable where T: Encodable { // expected-note {{where 'T' = 'OnlyDec'}}
}
extension Conditional: Decodable where T: Decodable { // expected-note {{where 'T' = 'OnlyEnc'}}
}

struct OnlyEnc: Encodable {}
struct OnlyDec: Decodable {}

// They should receive synthesized init(from:) and an encode(to:).
let _ = Conditional<OnlyDec>.init(from:)
let _ = Conditional<OnlyEnc>.encode(to:)

// but only for the appropriately *codable parameters.
let _ = Conditional<OnlyEnc>.init(from:) // expected-error {{referencing initializer 'init(from:)' on 'Conditional' requires that 'OnlyEnc' conform to 'Decodable'}}
let _ = Conditional<OnlyDec>.encode(to:) // expected-error {{referencing instance method 'encode(to:)' on 'Conditional' requires that 'OnlyDec' conform to 'Encodable'}}

// The synthesized CodingKeys type should not be accessible from outside the
// enum.
let _ = Conditional<Int>.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}
let _ = Conditional<Int>.ACodingKeys.self // expected-error {{'ACodingKeys' is inaccessible due to 'private' protection level}}
let _ = Conditional<Int>.BCodingKeys.self // expected-error {{'BCodingKeys' is inaccessible due to 'private' protection level}}
