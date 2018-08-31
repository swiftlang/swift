// RUN: %target-swift-frontend %s -emit-silgen

protocol FooProtocol {
  associatedtype Element
}

protocol Bar {
  associatedtype Foo : FooProtocol
  typealias Element = Foo.Element

  mutating func extend<
    C : FooProtocol
  >(elements: C)
    where
    C.Element == Element
}

struct FooImpl<T> : FooProtocol {
  typealias Element = T
}

struct BarImpl<T> : Bar {
  typealias Foo = FooImpl<T>

  // Uncomment this line to make it compile:
  // typealias Element = T

  mutating func extend<
    C : FooProtocol
  >(elements: C)
    where
    C.Element == T {}
}

