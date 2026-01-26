public protocol P {
  var foo: String { get }
}

internal protocol Q {}

extension Q {
  public var foo: String { "" }
  // expected-note@-1 {{mark the property as 'public' to satisfy the requirement}}
}

extension Optional: Q {}

extension Optional: P where Wrapped: P {}
// expected-warning@-1 {{property 'foo' must be as accessible as its enclosing type because it matches a requirement in protocol 'P'}}