// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -verify

// Codable enum with non-Codable parameter.
enum E1 : Codable {
// expected-error@-1 {{type 'E1' does not conform to protocol 'Decodable'}}
// expected-error@-2 {{type 'E1' does not conform to protocol 'Encodable'}}

  struct Nested {}
  case x(
    a: String = "",
    b: Int = 0,
    c: Nested = Nested())
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'Nested' does not conform to 'Decodable'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'Nested' does not conform to 'Encodable'}}
}

// Codable enum with non-enum CodingKeys.
enum E2 : Codable {
// expected-error@-1 {{type 'E2' does not conform to protocol 'Decodable'}}
// expected-error@-2 {{type 'E2' does not conform to protocol 'Encodable'}}

  case x(
      a: String = "",
      b: Int = 0,
      c: Double?)

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
enum E3 : Codable {
// expected-error@-1 {{type 'E3' does not conform to protocol 'Decodable'}}
// expected-error@-2 {{type 'E3' does not conform to protocol 'Encodable'}}

  case x(
      a: String = "",
      b: Int = 0,
      c: Double?)

  enum XCodingKeys : String {
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'CodingKeys' does not conform to CodingKey}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' does not conform to CodingKey}}
    case a
    case b
    case c
  }
}

// Codable enum with extraneous CodingKeys
enum E4 : Codable {
// expected-error@-1 {{type 'E4' does not conform to protocol 'Decodable'}}
// expected-error@-2 {{type 'E4' does not conform to protocol 'Encodable'}}

  case x(
      a: String = "",
      b: Int = 0,
      c: Double?)

  enum XCodingKeys : String, CodingKey {
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

// Codable enum with non-decoded parameter (which has no default value).
enum E5 : Codable {
// expected-error@-1 {{type 'E5' does not conform to protocol 'Decodable'}}

  case x(
      a: String = "",
      b: Int,
      // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'b' does not have a matching CodingKey and does not have a default value}}
      c: Double?)

  enum XCodingKeys : String, CodingKey {
    case a
    case c
  }
}

struct NotCodable {}
enum E6 {
  case x(
    a: NotCodable = NotCodable())
    // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'NotCodable' does not conform to 'Encodable'}}
    // expected-note@-2 {{cannot automatically synthesize 'Decodable' because 'NotCodable' does not conform to 'Decodable'}}
}
extension E6: Codable {}
// expected-error@-1 {{type 'E6' does not conform to protocol 'Encodable'}}
// expected-error@-2 {{type 'E6' does not conform to protocol 'Decodable'}}
