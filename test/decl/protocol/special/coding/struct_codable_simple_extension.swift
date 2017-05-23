// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Simple structs where Codable conformance is added in extensions should not
// derive conformance yet.
struct SimpleStruct { // expected-note {{did you mean 'init(x:y:)'?}}
  var x: Int
  var y: Double
  static var z: String = "foo"

  func foo() {
    // They should not receive a synthesized CodingKeys enum.
    let _ = SimpleStruct.CodingKeys.self // expected-error {{type 'SimpleStruct' has no member 'CodingKeys'}}
    let _ = SimpleStruct.CodingKeys.x // expected-error {{type 'SimpleStruct' has no member 'CodingKeys'}}
    let _ = SimpleStruct.CodingKeys.y // expected-error {{type 'SimpleStruct' has no member 'CodingKeys'}}
    let _ = SimpleStruct.CodingKeys.z // expected-error {{type 'SimpleStruct' has no member 'CodingKeys'}}
  }
}

extension SimpleStruct : Codable {} // expected-error {{implementation of 'Decodable' cannot be automatically synthesized in an extension}}
// expected-error@-1 {{implementation of 'Decodable' cannot be automatically synthesized in an extension}}
// expected-error@-2 {{implementation of 'Encodable' cannot be automatically synthesized in an extension}}
// expected-error@-3 {{implementation of 'Encodable' cannot be automatically synthesized in an extension}}

// They should not receive Codable methods.
let _ = SimpleStruct.init(from:) // expected-error {{type 'SimpleStruct' has no member 'init(from:)'}}
let _ = SimpleStruct.encode(to:) // expected-error {{type 'SimpleStruct' has no member 'encode(to:)'}}

// They should not get a CodingKeys type.
let _ = SimpleStruct.CodingKeys.self // expected-error {{type 'SimpleStruct' has no member 'CodingKeys'}}
