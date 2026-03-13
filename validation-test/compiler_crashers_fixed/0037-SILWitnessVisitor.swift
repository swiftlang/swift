// RUN: not %target-swift-frontend %s -emit-silgen

protocol FooType {
  func bar(b: Int)
}

extension FooType {
  func bar(a: Int, b: Int) {}
}

struct Foo : FooType {}
