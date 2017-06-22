// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s 2>&1 | %FileCheck %s

// Codable class with non-Codable property.
class C1 : Codable {
  class Nested {}
  var a: String = ""
  var b: Int = 0
  var c: Nested = Nested()

  // CHECK: error: type 'C1' does not conform to protocol 'Decodable'
  // CHECK: note: cannot automatically synthesize 'Decodable' because 'C1.Nested' does not conform to 'Decodable'
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'

  // CHECK: error: type 'C1' does not conform to protocol 'Encodable'
  // CHECK: note: cannot automatically synthesize 'Encodable' because 'C1.Nested' does not conform to 'Encodable'
  // CHECK: note: protocol requires function 'encode(to:)' with type 'Encodable'
}

// Codable class with non-enum CodingKeys.
class C2 : Codable {
  var a: String = ""
  var b: Int = 0
  var c: Double?

  struct CodingKeys : CodingKey {
    var stringValue = ""
    var intValue = nil
    init?(stringValue: String) {}
    init?(intValue: Int) {}
  }

  // CHECK: error: type 'C2' does not conform to protocol 'Decodable'
  // CHECK: note: cannot automatically synthesize 'Decodable' because 'CodingKeys' is not an enum
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'

  // CHECK: error: type 'C2' does not conform to protocol 'Encodable'
  // CHECK: note: cannot automatically synthesize 'Encodable' because 'CodingKeys' is not an enum
  // CHECK: note: protocol requires function 'encode(to:)' with type 'Encodable'
}

// Codable class with CodingKeys not conforming to CodingKey.
class C3 : Codable {
  var a: String = ""
  var b: Int = 0
  var c: Double?

  enum CodingKeys : String {
    case a
    case b
    case c
  }

  // CHECK: error: type 'C3' does not conform to protocol 'Decodable'
  // CHECK: note: cannot automatically synthesize 'Decodable' because 'CodingKeys' does not conform to CodingKey
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'

  // CHECK: error: type 'C3' does not conform to protocol 'Encodable'
  // CHECK: note: cannot automatically synthesize 'Encodable' because 'CodingKeys' does not conform to CodingKey
  // CHECK: note: protocol requires function 'encode(to:)' with type 'Encodable'
}

// Codable class with extraneous CodingKeys
class C4 : Codable {
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

  // CHECK: error: type 'C4' does not conform to protocol 'Decodable'
  // CHECK: note: CodingKey case 'a2' does not match any stored properties
  // CHECK: note: CodingKey case 'b2' does not match any stored properties
  // CHECK: note: CodingKey case 'c2' does not match any stored properties
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'

  // CHECK: error: type 'C4' does not conform to protocol 'Encodable'
  // CHECK: note: CodingKey case 'a2' does not match any stored properties
  // CHECK: note: CodingKey case 'b2' does not match any stored properties
  // CHECK: note: CodingKey case 'c2' does not match any stored properties
  // CHECK: note: protocol requires function 'encode(to:)' with type 'Encodable'
}

// Codable class with non-decoded property (which has no default value).
class C5 : Codable {
  var a: String = ""
  var b: Int
  var c: Double?

  enum CodingKeys : String, CodingKey {
    case a
    case c
  }

  // CHECK: error: class 'C5' has no initializers
  // CHECK: note: stored property 'b' without initial value prevents synthesized initializers

  // CHECK: error: type 'C5' does not conform to protocol 'Decodable'
  // CHECK: note: cannot automatically synthesize 'Decodable' because 'b' does not have a matching CodingKey and does not have a default value
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'
}

// Codable class with non-decoded property (which has no default value).
class C6 : Codable {
  var a: String = ""
  var b: Int
  var c: Double?

  enum CodingKeys : String, CodingKey {
    case a
    case c
  }

  init() {
    b = 5
  }

  // CHECK: error: type 'C6' does not conform to protocol 'Decodable'
  // CHECK: note: cannot automatically synthesize 'Decodable' because 'b' does not have a matching CodingKey and does not have a default value
  // CHECK: note: protocol requires initializer 'init(from:)' with type 'Decodable'
}

// Classes cannot yet synthesize Encodable or Decodable in extensions.
class C7 {}
extension C7 : Codable {}
// CHECK: error: implementation of 'Decodable' cannot be automatically synthesized in an extension yet
// CHECK: error: implementation of 'Encodable' cannot be automatically synthesized in an extension yet
