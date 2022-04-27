// RUN: %target-swift-frontend -emit-sil -verify %s

import Swift

// expected-error @+2 {{type 'Foo' does not conform to protocol 'Decodable'}}
// expected-error @+1 {{type 'Foo' does not conform to protocol 'Encodable'}}
enum Foo: Codable {
  // expected-note @+2 {{cannot automatically synthesize 'Decodable' for 'Foo' because user defined parameter name '_0' in 'foo' conflicts with automatically generated parameter name}}
  // expected-note @+1 {{cannot automatically synthesize 'Encodable' for 'Foo' because user defined parameter name '_0' in 'foo' conflicts with automatically generated parameter name}}
  case foo(Int, _0: String)
}

// Original set of crashers detected from PR-39825:

// expected-error @+2 {{type 'Foo2' does not conform to protocol 'Decodable'}}
// expected-error @+1 {{type 'Foo2' does not conform to protocol 'Encodable'}}
enum Foo2: Codable {
  // expected-note @+2 {{cannot automatically synthesize 'Decodable' for 'Foo2' because user defined parameter name 'a0' in 'foo' conflicts with automatically generated parameter name}}
  // expected-note @+1 {{cannot automatically synthesize 'Encodable' for 'Foo2' because user defined parameter name 'a0' in 'foo' conflicts with automatically generated parameter name}}
  case foo(Int, a0: String)
}

// expected-error @+2 {{type 'Foo3' does not conform to protocol 'Decodable'}}
// expected-error @+1 {{type 'Foo3' does not conform to protocol 'Encodable'}}
enum Foo3: Codable {
  // expected-note @+2 {{cannot automatically synthesize 'Decodable' for 'Foo3' because user defined parameter name 'a2' in 'foo' conflicts with automatically generated parameter name}}
  // expected-note @+1 {{cannot automatically synthesize 'Encodable' for 'Foo3' because user defined parameter name 'a2' in 'foo' conflicts with automatically generated parameter name}}
  case foo(Int, String, String, a2: String)
}

// expected-error @+2 {{type 'Foo4' does not conform to protocol 'Decodable'}}
// expected-error @+1 {{type 'Foo4' does not conform to protocol 'Encodable'}}
enum Foo4: Codable {
  // expected-note @+2 {{cannot automatically synthesize 'Decodable' for 'Foo4' because user defined parameter name 'a2' in 'foo' conflicts with automatically generated parameter name}}
  // expected-note @+1 {{cannot automatically synthesize 'Encodable' for 'Foo4' because user defined parameter name 'a2' in 'foo' conflicts with automatically generated parameter name}}
  case foo(String, String, String, a2: String)
}

// Valid code that successfully compiled in PR-39825

enum Foo5: Codable {
  case foo(_0: Int, String)
}

enum Foo6: Codable {
  case foo(_0: Int, _1: String)
}

enum Foo7: Codable {
  case foo(Int, _1: String)
}

enum Foo8: Codable {
  case foo(_1: Int, _0: String)
}

// Cases relating to `a0`:

enum Foo9 {
  case foo(Int, a0: String)
}

enum Foo10: Codable {
  case foo(Int, _a0: String)
}

// expected-error @+2 {{type 'Foo11' does not conform to protocol 'Decodable'}}
// expected-error @+1 {{type 'Foo11' does not conform to protocol 'Encodable'}}
enum Foo11: Codable {
  // expected-note @+2 {{cannot automatically synthesize 'Decodable' for 'Foo11' because user defined parameter name 'a1' in 'foo' conflicts with automatically generated parameter name}}
  // expected-note @+1 {{cannot automatically synthesize 'Encodable' for 'Foo11' because user defined parameter name 'a1' in 'foo' conflicts with automatically generated parameter name}}
  case foo(a1: Int, String)
}

enum Foo12: Codable {
  case foo(a0: Int, qt: String)
}

enum Foo13: Codable {
  case foo(qt: Int, a0: String)
}

enum Foo14: Codable {
  case foo(_1: Int, a0: String)
}

enum Foo15: Codable {
  case foo(_0: Int, a0: String)
}

enum Foo16: Codable {
  case foo(Int, b0: String)
}
