// RUN: %target-swift-frontend -primary-file %s -emit-ir %S/Inputs/rdar48049725_other.swift | %FileCheck %s
public protocol P3 {
  associatedtype A1: SomeClass
}


public protocol P2 {
  associatedtype A2: P3
}


func test<T: P5>(value: T) {
  // Ensure that we get the right generic signature for Foo.f
  // CHECK: call swiftcc void @"$s12rdar480497253FooV1fyyxAA2P5RzlFZ"
  Foo.f(value)
}
