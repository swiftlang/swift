// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s 2>&1 | %FileCheck %s

// Codable struct with non-Codable property.
struct S1 : Codable {
  struct Nested {}
  var a: String = ""
  var b: Int = 0
  var c: Nested = Nested()

  // CHECK: error: type 'S1' does not conform to protocol 'Decodable'
  // CHECK: note: cannot automatically synthesize 'Decodable' because 'S1.Nested' does not conform to 'Decodable'
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'

  // CHECK: error: type 'S1' does not conform to protocol 'Encodable'
  // CHECK: note: cannot automatically synthesize 'Encodable' because 'S1.Nested' does not conform to 'Encodable'
  // CHECK: note: protocol requires function 'encode(to:)' with type 'Encodable'
}

// Codable struct with non-enum CodingKeys.
struct S2 : Codable {
  var a: String = ""
  var b: Int = 0
  var c: Double?

  struct CodingKeys : CodingKey {
    var stringValue: String = ""
    var intValue: Int? = nil
    init?(stringValue: String) {}
    init?(intValue: Int) {}
  }

  // CHECK: error: type 'S2' does not conform to protocol 'Decodable'
  // CHECK: note: cannot automatically synthesize 'Decodable' because 'CodingKeys' is not an enum
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'

  // CHECK: error: type 'S2' does not conform to protocol 'Encodable'
  // CHECK: note: cannot automatically synthesize 'Encodable' because 'CodingKeys' is not an enum
  // CHECK: note: protocol requires function 'encode(to:)' with type 'Encodable'
}

// Codable struct with CodingKeys not conforming to CodingKey.
struct S3 : Codable {
  var a: String = ""
  var b: Int = 0
  var c: Double?

  enum CodingKeys : String {
    case a
    case b
    case c
  }

  // CHECK: error: type 'S3' does not conform to protocol 'Decodable'
  // CHECK: note: cannot automatically synthesize 'Decodable' because 'CodingKeys' does not conform to CodingKey
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'

  // CHECK: error: type 'S3' does not conform to protocol 'Encodable'
  // CHECK: note: cannot automatically synthesize 'Encodable' because 'CodingKeys' does not conform to CodingKey
  // CHECK: note: protocol requires function 'encode(to:)' with type 'Encodable'
}

// Codable struct with extraneous CodingKeys
struct S4 : Codable {
  var a: String = ""
  var b: Int = 0
  var c: Double?

  enum CodingKeys : String, CodingKey {
    case a
    case a2
    case b
    case b2
    case c
    case c2
  }

  // CHECK: error: type 'S4' does not conform to protocol 'Decodable'
  // CHECK: note: CodingKey case 'a2' does not match any stored properties
  // CHECK: note: CodingKey case 'b2' does not match any stored properties
  // CHECK: note: CodingKey case 'c2' does not match any stored properties
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'

  // CHECK: error: type 'S4' does not conform to protocol 'Encodable'
  // CHECK: note: CodingKey case 'a2' does not match any stored properties
  // CHECK: note: CodingKey case 'b2' does not match any stored properties
  // CHECK: note: CodingKey case 'c2' does not match any stored properties
  // CHECK: note: protocol requires function 'encode(to:)' with type 'Encodable'
}

// Codable struct with non-decoded property (which has no default value).
struct S5 : Codable {
  var a: String = ""
  var b: Int
  var c: Double?

  enum CodingKeys : String, CodingKey {
    case a
    case c
  }

  // CHECK: error: type 'S5' does not conform to protocol 'Decodable'
  // CHECK: note: cannot automatically synthesize 'Decodable' because 'b' does not have a matching CodingKey and does not have a default value
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'
}

// Structs cannot yet synthesize Encodable or Decodable in extensions.
struct S6 {}
extension S6 : Codable {}
// CHECK: error: implementation of 'Decodable' cannot be automatically synthesized in an extension yet
// CHECK: error: implementation of 'Encodable' cannot be automatically synthesized in an extension yet
