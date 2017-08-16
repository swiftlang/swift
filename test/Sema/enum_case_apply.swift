// RUN: %target-typecheck-verify-swift -swift-version 5

enum E1 {
  case payload(a: Int) // expected-note {{found this candidate}}
  case payload(b: Int) // expected-note {{found this candidate}}
  case payload(a: Int, b: Int)
}

func testCompoundEnumCase(a: Int, b: Int) {
  _ = E1.payload(a: a) // okay: direct call requires argument labels
  _ = E1.payload(b: a) // okay: direct call requires argument labels
  _ = E1.payload(a: a, b: a) // okay: direct call requires argument labels
  _ = (E1.payload)(a: a, b: a) // okay: direct call requires argument labels
  _ = ((E1.payload))(a: a, b: a) // okay: direct call requires argument labels

  _ = E1.payload(a:b:)(a, a) // compound name suppresses argument labels

  let _: E1 = .payload(a: a) // okay: direct call requires argument labels
  let _: E1 = .payload(b: a) // okay: direct call requires argument labels
  let _: E1 = .payload(a: a, b: a) // okay: direct call requires argument labels

  let _: (Int, Int) -> E1 = E1.payload // contextual types can pick out members without labels

  // ambiguity
  let _: (Int) -> E1 = E1.payload // expected-error {{ambiguous use of 'payload'}}
  // match the behavior of contextual applies in functions
  let _ : E1 = .payload(a:b:)(a, a) // expected-error {{missing argument labels 'a:b:' in call}}
}
