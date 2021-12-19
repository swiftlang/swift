// RUN: %target-swift-frontend -emit-ir -primary-file %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

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

// CHECK-LABEL: sr11639.(file).BazProtocol@
// CHECK-NEXT: Requirement signature: <Self where Self : FooProtocol, Self.Foo1 : FooProtocol, Self.Foo2Bar == Self.Foo1.Bar>