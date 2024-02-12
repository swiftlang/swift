public protocol P {
  associatedtype A : P
  func d() -> Self.A
}

public struct B<Content: P> {
  public var a: Content.A
  init(_ v: Content) {
    a = v.d()
  }
}
