public protocol Foo {}
extension Bool: Foo {}

@_rawLayout(like: T)
public struct Fred<T: Foo>: ~Copyable {
  public init() {}
}
