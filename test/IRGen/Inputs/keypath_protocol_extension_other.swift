public protocol P<A> {
  associatedtype A
}

extension P {
  public var value: Bool { true }
}
