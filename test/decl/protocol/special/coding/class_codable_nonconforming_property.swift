// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

struct NonCodable {}

// Classes whose properties are not all Codable should fail to synthesize
// conformance.
class NonConformingClass : Codable { // expected-error {{type 'NonConformingClass' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'NonConformingClass' does not conform to protocol 'Decodable'}}
  // expected-error@-2 {{type 'NonConformingClass' does not conform to protocol 'Encodable'}}
  // expected-error@-3 {{type 'NonConformingClass' does not conform to protocol 'Encodable'}}
  // expected-note@-4 {{did you mean 'init'?}}
  var w: NonCodable = NonCodable() // expected-note {{cannot automatically synthesize 'Decodable' because 'w' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'w' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'w' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'w' does not conform to 'Encodable'}}
  var x: Int = 1
  var y: Double = .pi
  static var z: String = "foo"

  // These lines have to be within the NonConformingClass type because
  // CodingKeys should be private.
  func foo() {
    // They should not get a CodingKeys type.
    let _ = NonConformingClass.CodingKeys.self // expected-error {{type 'NonConformingClass' has no member 'CodingKeys'}}
    let _ = NonConformingClass.CodingKeys.x // expected-error {{type 'NonConformingClass' has no member 'CodingKeys'}}
    let _ = NonConformingClass.CodingKeys.y // expected-error {{type 'NonConformingClass' has no member 'CodingKeys'}}
    let _ = NonConformingClass.CodingKeys.z // expected-error {{type 'NonConformingClass' has no member 'CodingKeys'}}
  }
}

// They should not receive Codable methods.
let _ = NonConformingClass.init(from:) // expected-error {{type 'NonConformingClass' has no member 'init(from:)'}}
let _ = NonConformingClass.encode(to:) // expected-error {{type 'NonConformingClass' has no member 'encode(to:)'}}

// They should not get a CodingKeys type.
let _ = NonConformingClass.CodingKeys.self // expected-error {{type 'NonConformingClass' has no member 'CodingKeys'}}
