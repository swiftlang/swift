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
struct SR_12248_1: Codable { 
  var x: Int // expected-note {{'x' previously declared here}}
  var x: Int // expected-error {{invalid redeclaration of 'x'}}
}

struct SR_12248_2: Decodable { 
  var x: Int // expected-note {{'x' previously declared here}}
  var x: Int // expected-error {{invalid redeclaration of 'x'}}
}

struct SR_12248_3: Encodable { 
  var x: Int // expected-note {{'x' previously declared here}}
  var x: Int // expected-error {{invalid redeclaration of 'x'}}
}

struct NotConforms: Codable { // expected-error {{type 'NotConforms' does not conform to protocol 'Decodable'}}
// expected-error@-1 {{type 'NotConforms' does not conform to protocol 'Encodable'}}
  enum CodingKeys: CodingKey {
    case x
    case y
  }
  var x: Int
  var y: NotDefined // expected-error {{cannot find type 'NotDefined' in scope}}
}

struct CodableDuplicatedKeys: Codable { 
  enum CodingKeys: CodingKey {
    case x // expected-note{{'x' previously declared here}}
    case x // expected-error{{invalid redeclaration of 'x'}}
  }
  var x: Int
}

struct NonCodable {}
// expected-error@+2 {{type 'DuplicatedKeys' does not conform to protocol 'Encodable'}}
// expected-error@+1 {{type 'DuplicatedKeys' does not conform to protocol 'Decodable'}}
struct DuplicatedKeys: Codable { 
  enum CodingKeys: CodingKey {
    case x // expected-note{{'x' previously declared here}}
    case x // expected-error{{invalid redeclaration of 'x'}}
  }
  var x: NonCodable 
  // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'NonCodable' does not conform to 'Encodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Decodable' because 'NonCodable' does not conform to 'Decodable'}}
}
