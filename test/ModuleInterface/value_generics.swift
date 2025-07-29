// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name ValueGeneric -disable-availability-checking
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name ValueGeneric -disable-availability-checking
// RUN: %FileCheck --implicit-check-not=ValueGenericsNameLookup %s < %t.swiftinterface

// CHECK: public struct Slab<Element, let N : Swift.Int>
public struct Slab<Element, let N: Int> {
  // CHECK-LABEL: public var count: Swift.Int {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      N
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }
  @inlinable
  public var count: Int {
    N
  }

  public init() {}
}

// CHECK: public func usesGenericSlab<let N : Swift.Int>(_: ValueGeneric.Slab<Swift.Int, N>)
public func usesGenericSlab<let N: Int>(_: Slab<Int, N>) {}

// CHECK: public func usesConcreteSlab(_: ValueGeneric.Slab<Swift.Int, 2>)
public func usesConcreteSlab(_: Slab<Int, 2>) {}

// CHECK: public func usesNegativeSlab(_: ValueGeneric.Slab<Swift.String, -10>)
public func usesNegativeSlab(_: Slab<String, -10>) {}

@inlinable
public func test() -> Int {
  // CHECK: Slab<Int, 123>.N
  Slab<Int, 123>.N
}

@inlinable
public func test2() -> Int {
  // CHECK: type(of: Slab<Int, 123>()).N
  type(of: Slab<Int, 123>()).N
}

@inlinable
public func test3() {
  {
    print(123)
    print(123)
    print(Slab<Int, 123>.N)
  }()
}
