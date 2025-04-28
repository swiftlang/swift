public struct G<T> {
  public init(_: T) {}
}

public protocol P {
  associatedtype A: P

  func a() -> A
}
