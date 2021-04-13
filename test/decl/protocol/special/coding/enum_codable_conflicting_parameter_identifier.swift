// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

enum Duplicate : Codable { // expected-error {{type 'Duplicate' does not conform to protocol 'Decodable'}}
  // expected-error@-1 {{type 'Duplicate' does not conform to protocol 'Encodable'}}
  case a(Int, Int, _0: Int, _1: Int)
  // expected-note@-1 {{cannot automatically synthesize 'Decodable' for 'Duplicate' because user defined parameter name '_0' in 'a' conflicts with automatically generated parameter name}}
  // expected-note@-2 {{cannot automatically synthesize 'Decodable' for 'Duplicate' because user defined parameter name '_1' in 'a' conflicts with automatically generated parameter name}}
  // expected-note@-3 {{cannot automatically synthesize 'Encodable' for 'Duplicate' because user defined parameter name '_0' in 'a' conflicts with automatically generated parameter name}}
  // expected-note@-4 {{cannot automatically synthesize 'Encodable' for 'Duplicate' because user defined parameter name '_1' in 'a' conflicts with automatically generated parameter name}}
}
