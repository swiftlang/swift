// RUN: %target-swift-frontend -emit-ir -primary-file %s -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/54050

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

// CHECK-LABEL: {{^.+}}.(file).BazProtocol@
// CHECK-NEXT: Requirement signature: <Self where Self : FooProtocol, Self.[BazProtocol]Foo1 : FooProtocol, Self.[BazProtocol]Foo2Bar == Self.[BazProtocol]Foo1.[FooProtocol]Bar>
