// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

struct Foo : Codable {
  let x1: String = ""
  // expected-warning@-1 {{immutable property will not be decoded because it is declared with an initial value which cannot be overwritten}}
  // expected-note@-2 {{set the initial value via the initializer or explicitly define a CodingKeys enum including a 'x1' case to silence this warning}}
  // expected-note@-3 {{make the property mutable instead}}{{3-6=var}}

  public let x2: String = ""
  // expected-warning@-1 {{immutable property will not be decoded because it is declared with an initial value which cannot be overwritten}}
  // expected-note@-2 {{set the initial value via the initializer or explicitly define a CodingKeys enum including a 'x2' case to silence this warning}}
  // expected-note@-3 {{make the property mutable instead}}{{10-13=var}}

  internal let x3: String = ""
  // expected-warning@-1 {{immutable property will not be decoded because it is declared with an initial value which cannot be overwritten}}
  // expected-note@-2 {{set the initial value via the initializer or explicitly define a CodingKeys enum including a 'x3' case to silence this warning}}
  // expected-note@-3 {{make the property mutable instead}}{{12-15=var}}

  fileprivate let x4: String = ""
  // expected-warning@-1 {{immutable property will not be decoded because it is declared with an initial value which cannot be overwritten}}
  // expected-note@-2 {{set the initial value via the initializer or explicitly define a CodingKeys enum including a 'x4' case to silence this warning}}
  // expected-note@-3 {{make the property mutable instead}}{{15-18=var}}

  private let x5: String = ""
  // expected-warning@-1 {{immutable property will not be decoded because it is declared with an initial value which cannot be overwritten}}
  // expected-note@-2 {{set the initial value via the initializer or explicitly define a CodingKeys enum including a 'x5' case to silence this warning}}
  // expected-note@-3 {{make the property mutable instead}}{{11-14=var}}
}
