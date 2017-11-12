// RUN: %target-typecheck-verify-swift

// Simple classes where Codable conformance is added in extensions should not
// derive conformance yet.
class SimpleClass { // expected-note {{did you mean 'init'?}}
  var x: Int = 1
  var y: Double = .pi
  static var z: String = "foo"

  func foo() {
    // They should not get a CodingKeys type.
    let _ = SimpleClass.CodingKeys.self // expected-error {{type 'SimpleClass' has no member 'CodingKeys'}}
    let _ = SimpleClass.CodingKeys.x // expected-error {{type 'SimpleClass' has no member 'CodingKeys'}}
    let _ = SimpleClass.CodingKeys.y // expected-error {{type 'SimpleClass' has no member 'CodingKeys'}}
    let _ = SimpleClass.CodingKeys.z // expected-error {{type 'SimpleClass' has no member 'CodingKeys'}}
  }
}

extension SimpleClass : Codable {} // expected-error {{implementation of 'Decodable' cannot be automatically synthesized in an extension}}
// expected-error@-1 {{implementation of 'Decodable' cannot be automatically synthesized in an extension}}
// expected-error@-2 {{implementation of 'Encodable' cannot be automatically synthesized in an extension}}
// expected-error@-3 {{implementation of 'Encodable' cannot be automatically synthesized in an extension}}

// They should not receive Codable methods.
let _ = SimpleClass.init(from:) // expected-error {{type 'SimpleClass' has no member 'init(from:)'}}
let _ = SimpleClass.encode(to:) // expected-error {{type 'SimpleClass' has no member 'encode(to:)'}}

// They should not get a CodingKeys type.
let _ = SimpleClass.CodingKeys.self // expected-error {{type 'SimpleClass' has no member 'CodingKeys'}}
