// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// SR-11996 - Warning when a immutable decodable property has an initial value

// Implicit CodingKeys enum //

struct Foo1: Codable {
  let bar: Int = 1
  // expected-warning@-1 {{immutable property will not be decoded because it is declared with an initial value which cannot be overwritten}}
  // expected-note@-2 {{set the initial value via the initializer or explicitly define a CodingKeys enum including a 'bar' case to silence this warning}}
  // expected-note@-3 {{make the property mutable instead}}{{3-6=var}}
  let baz1: String
  var baz2 = 8
}

struct Foo2: Decodable {
  let bar: Int = 1
  // expected-warning@-1 {{immutable property will not be decoded because it is declared with an initial value which cannot be overwritten}}
  // expected-note@-2 {{set the initial value via the initializer or explicitly define a CodingKeys enum without a 'bar' case to silence this warning}}
  // expected-note@-3 {{make the property mutable instead}}{{3-6=var}}
  let baz1: String
  var baz2 = 8
}

struct Foo3: Encodable {
  let bar: Int = 1 // Ok, since 'Foo3' isn't decodable
  let baz1: String
  var baz2 = 8
}

// Explicit CodingKeys enum //

struct Foo4: Codable {
  let bar: Int = 1 // Ok, since removing 'bar' from 'CodingKeys' will break encoding
  let baz1: String
  var baz2 = 8

  private enum CodingKeys: String, CodingKey {
    case bar
    case baz1
    case baz2
  }
}

struct Foo5: Decodable {
  let bar: Int = 1
  // expected-warning@-1 {{immutable property will not be decoded because it is declared with an initial value which cannot be overwritten}}
  // expected-note@-2 {{set the initial value via the initializer or remove the 'bar' case from the CodingKeys enum to silence this warning}}
  // expected-note@-3 {{make the property mutable instead}}{{3-6=var}}
  let baz1: String
  var baz2 = 8

  private enum CodingKeys: String, CodingKey {
    case bar
    case baz1
    case baz2
  }
}

struct Foo6: Encodable {
  let bar: Int = 1 // Ok, since 'Foo6' isn't decodable
  let baz1: String
  var baz2 = 8

  private enum CodingKeys: String, CodingKey {
    case bar
    case baz1
    case baz2
  }
}

struct Foo7: Codable {
  let bar: Int = 1 // Ok, since 'bar' doesn't exist in CodingKeys enum
  let baz1: String
  var baz2 = 8

  private enum CodingKeys: String, CodingKey {
    case baz1
    case baz2
  }
}

struct Foo8: Decodable {
  let bar: Int = 1 // Ok, since 'bar' does not exist in CodingKeys enum
  let baz1: String
  var baz2 = 8

  private enum CodingKeys: String, CodingKey {
    case baz1
    case baz2
  }
}

struct Foo9: Encodable {
  let bar: Int = 1 // Ok, since 'Foo9' isn't decodable (and 'bar' doesn't exist in CodingKeys enum anyway)
  let baz1: String
  var baz2 = 8

  private enum CodingKeys: String, CodingKey {
    case baz1
    case baz2
  }
}
