// RUN: %target-swift-frontend %s -emit-silgen

protocol FooType {
  typealias Element
}

protocol BarType {
  typealias Foo : FooType
  typealias Element = Foo.Element

  mutating func extend<
    C : FooType
    where
    C.Element == Element
  >(elements: C)
}

struct FooImpl<T> : FooType {
  typealias Element = T
}

struct BarImpl<T> : BarType {
  typealias Foo = FooImpl<T>

  // Uncomment this line to make it compile:
  // typealias Element = T

  mutating func extend<
    C : FooType
    where
    C.Element == T
  >(elements: C) {}
}

