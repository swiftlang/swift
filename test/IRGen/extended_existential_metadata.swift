// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -O %s | %FileCheck %s

protocol Foo<T> {
  associatedtype T
  func acceptEvent<T>(event: T)
}

protocol FooFactory<T> {
  associatedtype T
  func makeFoo() -> any Foo<T>
}

class Bar<T> {
  private var foo: (any Foo<T>)

  init(fooFactory: any FooFactory<T>) {
      self.foo = fooFactory.makeFoo()
  }
}

// CHECK-NOT: swift_getExtendedExistentialTypeMetadata
