public protocol Foo {}

public struct FooImpl: Foo {
  public init() {}
}

public func anyFoo() -> some Foo {
  return FooImpl()
}

public var anyFooProp: some Foo {
  return FooImpl()
}

public struct Subscript {
  public init() {}

  public subscript() -> some Foo {
    return FooImpl()
  }
}

extension Foo {
  public func identity<T>(_: T) -> some Foo {
    return self
  }
}
