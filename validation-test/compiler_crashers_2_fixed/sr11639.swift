// RUN: %target-swift-frontend -emit-ir -primary-file %s

public protocol FooProtocol {
  associatedtype Bar
}

public struct Foo<Bar>: FooProtocol {
  public var bar: Bar
}

public protocol BazProtocol: FooProtocol {
  associatedtype Foo1: FooProtocol where Foo1.Bar == Foo2.Bar
  associatedtype Foo2Bar
  typealias Foo2 = Foo<Foo2Bar>
}
