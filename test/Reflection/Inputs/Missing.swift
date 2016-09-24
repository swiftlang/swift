public struct Box<T> {
  public let value: T
}

public protocol P {
  associatedtype A
  associatedtype B
}

public struct Bar<T : P> {
  public let a: T.A
  public let b: T.B
  public let c: (T.A, T.B)
}
