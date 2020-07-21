// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Simple structs with all Codable properties should get derived conformance to
// Codable.
struct SimpleStruct : Codable {
    var x: Int
    var y: Double
    static var z: String = "foo"

    // These lines have to be within the SimpleStruct type because CodingKeys
    // should be private.
    func foo() {
        // They should receive a synthesized CodingKeys enum.
        let _ = SimpleStruct.CodingKeys.self

        // The enum should have a case for each of the vars.
        let _ = SimpleStruct.CodingKeys.x
        let _ = SimpleStruct.CodingKeys.y

        // Static vars should not be part of the CodingKeys enum.
        let _ = SimpleStruct.CodingKeys.z // expected-error {{type 'SimpleStruct.CodingKeys' has no member 'z'}}
    }
}

// They should receive synthesized init(from:) and an encode(to:).
let _ = SimpleStruct.init(from:)
let _ = SimpleStruct.encode(to:)

// The synthesized CodingKeys type should not be accessible from outside the
// struct.
let _ = SimpleStruct.CodingKeys.self // expected-error {{'CodingKeys' is inaccessible due to 'private' protection level}}

// rdar://problem/59655704 
struct SR_12248_1: Codable { // expected-error {{type 'SR_12248_1' does not conform to protocol 'Encodable'}}
  var x: Int // expected-note {{'x' previously declared here}}
  var x: Int // expected-error {{invalid redeclaration of 'x'}}
  // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'Int' does not conform to 'Encodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'Int' does not conform to 'Encodable'}}
}

struct SR_12248_2: Decodable { 
  var x: Int // expected-note {{'x' previously declared here}}
  var x: Int // expected-error {{invalid redeclaration of 'x'}}
}

struct SR_12248_3: Encodable { 
  var x: Int // expected-note {{'x' previously declared here}}
  var x: Int // expected-error {{invalid redeclaration of 'x'}}
}
