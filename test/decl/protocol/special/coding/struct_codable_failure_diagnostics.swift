// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -verify

// Codable struct with non-Codable property.
struct S1 : Codable {
// expected-error@-1 {{type 'S1' does not conform to protocol 'Decodable'}}
// expected-error@-2 {{type 'S1' does not conform to protocol 'Encodable'}}

  struct Nested {}
  var a: String = ""
  var b: Int = 0
  var c: Nested = Nested()
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'S1.Nested' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'S1.Nested' does not conform to 'Encodable'}}
}

// Codable struct with non-enum CodingKeys.
struct S2 : Codable {
// expected-error@-1 {{type 'S2' does not conform to protocol 'Decodable'}}
// expected-error@-2 {{type 'S2' does not conform to protocol 'Encodable'}}

  var a: String = ""
  var b: Int = 0
  var c: Double?

  struct CodingKeys : CodingKey {
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'CodingKeys' is not an enum}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' is not an enum}}
    var stringValue: String = ""
    var intValue: Int? = nil
    init?(stringValue: String) {}
    init?(intValue: Int) {}
  }
}

// Codable struct with CodingKeys not conforming to CodingKey.
struct S3 : Codable {
// expected-error@-1 {{type 'S3' does not conform to protocol 'Decodable'}}
// expected-error@-2 {{type 'S3' does not conform to protocol 'Encodable'}}

  var a: String = ""
  var b: Int = 0
  var c: Double?

  enum CodingKeys : String {
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'CodingKeys' does not conform to CodingKey}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' does not conform to CodingKey}}
    case a
    case b
    case c
  }
}

// Codable struct with extraneous CodingKeys
struct S4 : Codable {
// expected-error@-1 {{type 'S4' does not conform to protocol 'Decodable'}}
// expected-error@-2 {{type 'S4' does not conform to protocol 'Encodable'}}

  var a: String = ""
  var b: Int = 0
  var c: Double?

  enum CodingKeys : String, CodingKey {
    case a
    case a2
    // expected-note@-1 {{CodingKey case 'a2' does not match any stored properties}}
    // expected-note@-2 {{CodingKey case 'a2' does not match any stored properties}}
    case b
    case b2
    // expected-note@-1 {{CodingKey case 'b2' does not match any stored properties}}
    // expected-note@-2 {{CodingKey case 'b2' does not match any stored properties}}
    case c
    case c2
    // expected-note@-1 {{CodingKey case 'c2' does not match any stored properties}}
    // expected-note@-2 {{CodingKey case 'c2' does not match any stored properties}}
  }
}

// Codable struct with non-decoded property (which has no default value).
struct S5 : Codable {
// expected-error@-1 {{type 'S5' does not conform to protocol 'Decodable'}}

  var a: String = ""
  var b: Int
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'b' does not have a matching CodingKey and does not have a default value}}
  var c: Double?

  enum CodingKeys : String, CodingKey {
    case a
    case c
  }
}

struct NotCodable {}
struct S6 {
    var x: NotCodable = NotCodable()
    // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'NotCodable' does not conform to 'Encodable'}}
    // expected-note@-2 {{cannot automatically synthesize 'Decodable' because 'NotCodable' does not conform to 'Decodable'}}
}
extension S6: Codable {}
// expected-error@-1 {{type 'S6' does not conform to protocol 'Encodable'}}
// expected-error@-2 {{type 'S6' does not conform to protocol 'Decodable'}}
