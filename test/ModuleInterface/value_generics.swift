// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name ValueGeneric -enable-experimental-feature ValueGenerics -disable-availability-checking -disable-experimental-parser-round-trip
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name ValueGeneric -disable-availability-checking
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: public struct Vector<Element, let N : Swift.Int>
public struct Vector<Element, let N: Int> {
  // CHECK-LABEL: public var count: Swift.Int {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      N
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }
  @inlinable
  public var count: Int {
    N
  }
}

// CHECK: public func usesGenericVector<let N : Swift.Int>(_: ValueGeneric.Vector<Swift.Int, N>)
public func usesGenericVector<let N: Int>(_: Vector<Int, N>) {}

// CHECK: public func usesConcreteVector(_: ValueGeneric.Vector<Swift.Int, 2>)
public func usesConcreteVector(_: Vector<Int, 2>) {}

// CHECK: public func usesNegativeVector(_: ValueGeneric.Vector<Swift.String, -10>)
public func usesNegativeVector(_: Vector<String, -10>) {}
