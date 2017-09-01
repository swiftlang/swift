extension SimpleClass : Codable {} // expected-error {{implementation of 'Decodable' can only be synthesized in the same source file as 'SimpleClass'}}
// expected-error@-1 {{implementation of 'Decodable' can only be synthesized in the same source file as 'SimpleClass'}}
// expected-error@-2 {{implementation of 'Encodable' can only be synthesized in the same source file as 'SimpleClass'}}
// expected-error@-3 {{implementation of 'Encodable' can only be synthesized in the same source file as 'SimpleClass'}}

// These are wrapped in a dummy function to avoid binding a global variable.
func foo() {
  // They should not receive Codable methods.
  let _ = SimpleClass.init(from:) // expected-error {{type 'SimpleClass' has no member 'init(from:)'}}
  let _ = SimpleClass.encode(to:) // expected-error {{type 'SimpleClass' has no member 'encode(to:)'}}

  // They should not get a CodingKeys type.
  let _ = SimpleClass.CodingKeys.self // expected-error {{type 'SimpleClass' has no member 'CodingKeys'}}
}
