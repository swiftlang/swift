public struct Outer<T> {
  public struct Inner<U> {
    public init() {}
  }
}

extension Outer.Inner {
  public func extensionMethod(t: T) -> U {
    while true {}
  }
}
