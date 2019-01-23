public protocol Foo {}

public struct FooImpl: Foo {
  public init() {}
}

public func anyFoo() -> __opaque Foo {
  return FooImpl()
}
