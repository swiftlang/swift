// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Classes with a CodingKeys entity which is not a type should not derive
// conformance.
class InvalidCodingKeys1 : Codable { // expected-error {{type 'InvalidCodingKeys1' does not conform to protocol 'Decodable'}}
// expected-error@-1 {{type 'InvalidCodingKeys1' does not conform to protocol 'Encodable'}}
  let CodingKeys = 5 // expected-note {{cannot automatically synthesize 'Decodable' because 'CodingKeys' is not an enum}}
  // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' is not an enum}}
}

// Classes with a CodingKeys entity which does not conform to CodingKey should
// not derive conformance.
class InvalidCodingKeys2 : Codable { // expected-error {{type 'InvalidCodingKeys2' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'InvalidCodingKeys2' does not conform to protocol 'Encodable'}}
  enum CodingKeys {} // expected-note {{cannot automatically synthesize 'Decodable' because 'CodingKeys' does not conform to CodingKey}}
  // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' does not conform to CodingKey}}
}

// Classes with a CodingKeys entity which is not an enum should not derive
// conformance.
class InvalidCodingKeys3 : Codable { // expected-error {{type 'InvalidCodingKeys3' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'InvalidCodingKeys3' does not conform to protocol 'Encodable'}}
  struct CodingKeys : CodingKey { // expected-note {{cannot automatically synthesize 'Decodable' because 'CodingKeys' is not an enum}}
    // expected-note@-1 {{cannot automatically synthesize 'Encodable' because 'CodingKeys' is not an enum}}
      var stringValue: String
      init?(stringValue: String) {
          self.stringValue = stringValue
          self.intValue = nil
      }

      var intValue: Int?
      init?(intValue: Int) {
          self.stringValue = "\(intValue)"
          self.intValue = intValue
      }
  }
}
