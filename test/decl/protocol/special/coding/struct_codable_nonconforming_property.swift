// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

struct NonCodable {}

// Structs whose properties are not all Codable should fail to synthesize
// conformance.
struct NonConformingStruct : Codable { // expected-error {{type 'NonConformingStruct' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'NonConformingStruct' does not conform to protocol 'Decodable'}}
  // expected-error@-2 {{type 'NonConformingStruct' does not conform to protocol 'Encodable'}}
  // expected-error@-3 {{type 'NonConformingStruct' does not conform to protocol 'Encodable'}}
  // expected-note@-4 {{did you mean 'init'?}}
  var w: NonCodable // expected-note {{cannot automatically synthesize 'Decodable' because 'w' does not conform to 'Decodable'}}
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'w' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'w' does not conform to 'Encodable'}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' because 'w' does not conform to 'Encodable'}}
  var x: Int
  var y: Double
  static var z: String = "foo"

  // These lines have to be within the NonConformingStruct type because
  // CodingKeys should be private.
  func foo() {
    // They should not get a CodingKeys type.
    let _ = NonConformingStruct.CodingKeys.self // expected-error {{type 'NonConformingStruct' has no member 'CodingKeys'}}
    let _ = NonConformingStruct.CodingKeys.x // expected-error {{type 'NonConformingStruct' has no member 'CodingKeys'}}
    let _ = NonConformingStruct.CodingKeys.y // expected-error {{type 'NonConformingStruct' has no member 'CodingKeys'}}
    let _ = NonConformingStruct.CodingKeys.z // expected-error {{type 'NonConformingStruct' has no member 'CodingKeys'}}
  }
}

// They should not receive Codable methods.
let _ = NonConformingStruct.init(from:) // expected-error {{type 'NonConformingStruct' has no member 'init(from:)'}}
let _ = NonConformingStruct.encode(to:) // expected-error {{type 'NonConformingStruct' has no member 'encode(to:)'}}

// They should not get a CodingKeys type.
let _ = NonConformingStruct.CodingKeys.self // expected-error {{type 'NonConformingStruct' has no member 'CodingKeys'}}
