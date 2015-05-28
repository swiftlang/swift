// RUN: not --crash %target-swift-frontend %s -emit-silgen

protocol FooType {
  typealias Element
}

protocol BarType {
  typealias Foo : FooType

  mutating func extend<
    C : FooType
    where
    C.Element == Foo.Element
  >(elements: C)
}

