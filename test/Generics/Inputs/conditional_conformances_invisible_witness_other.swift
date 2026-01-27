public protocol P {
  var foo: String { get }
}

internal protocol Q {}

extension Q {
  public var foo: String { "" }
}

extension Optional: Q {}

extension Optional: P where Wrapped: P {}
