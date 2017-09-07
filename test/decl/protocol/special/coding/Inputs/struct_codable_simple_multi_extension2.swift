extension SimpleStruct : Codable {} // expected-error {{implementation of 'Decodable' can only be synthesized in the same source file as 'SimpleStruct'}}
// expected-error@-1 {{implementation of 'Decodable' can only be synthesized in the same source file as 'SimpleStruct'}}
// expected-error@-2 {{implementation of 'Encodable' can only be synthesized in the same source file as 'SimpleStruct'}}
// expected-error@-3 {{implementation of 'Encodable' can only be synthesized in the same source file as 'SimpleStruct'}}

// These are wrapped in a dummy function to avoid binding a global variable.
func foo() {
  // They should not receive Codable methods.
  let _ = SimpleStruct.init(from:) // expected-error {{type 'SimpleStruct' has no member 'init(from:)'}}
  let _ = SimpleStruct.encode(to:) // expected-error {{type 'SimpleStruct' has no member 'encode(to:)'}}

  // They should not get a CodingKeys type.
  let _ = SimpleStruct.CodingKeys.self // expected-error {{type 'SimpleStruct' has no member 'CodingKeys'}}
}
