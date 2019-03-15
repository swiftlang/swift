public protocol Foo {}

public struct FooImpl: Foo {
  public init() {}
}

public func anyFoo() -> some Foo {
  return FooImpl()
}
