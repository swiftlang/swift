// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Simple enums with all Codable parameters whose CodingKeys come from a
// typealias should get derived conformance to Codable.
enum SimpleEnum : Codable {
  // expected-error@-1 {{type 'SimpleEnum' does not conform to protocol 'Decodable'}}
  // expected-error@-2 {{type 'SimpleEnum' does not conform to protocol 'Encodable'}}
  case x(x: Int)
  case x(x: Int, y: String)
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'SimpleEnum' has duplicate case name 'x'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'SimpleEnum' has duplicate case name 'x'}}

  case y(x: Int)

  case z(x: Int)
  case z(x: Int, y: String)
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'SimpleEnum' has duplicate case name 'z'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'SimpleEnum' has duplicate case name 'z'}}
  case z(x: Int, y: String, z: Bool)
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' because 'SimpleEnum' has duplicate case name 'z'}}
  // expected-note@-2 {{cannot automatically synthesize 'Encodable' because 'SimpleEnum' has duplicate case name 'z'}}
}
