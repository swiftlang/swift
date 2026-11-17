// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name ValueGeneric -target %target-swift-6.2-abi-triple
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name ValueGeneric -target %target-swift-6.2-abi-triple
// RUN: %FileCheck --implicit-check-not=ValueGenericsNameLookup %s < %t.swiftinterface

// CHECK: public struct Slab<Element, let N : Swift::Int>
public struct Slab<Element, let N: Int> {
  // CHECK-LABEL: public var count: Swift::Int {
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

// CHECK: public func usesGenericSlab<let N : Swift::Int>(_: ValueGeneric::Slab<Swift::Int, N>)
public func usesGenericSlab<let N: Int>(_: Slab<Int, N>) {}

// CHECK: public func usesConcreteSlab(_: ValueGeneric::Slab<Swift::Int, 2>)
public func usesConcreteSlab(_: Slab<Int, 2>) {}

// CHECK: public func usesNegativeSlab(_: ValueGeneric::Slab<Swift::String, -10>)
public func usesNegativeSlab(_: Slab<String, -10>) {}

public protocol P {}
public struct ConformsToP: P {
  public init() {}
}

public protocol Q {
  associatedtype A: P
  func make() -> A
}

// CHECK-LABEL: extension ValueGeneric::Slab : ValueGeneric::Q {
extension Slab: Q {
  // CHECK: public func make() -> some ValueGeneric::P
  public func make() -> some P { ConformsToP() }

  // CHECK: public typealias A = @_opaqueReturnTypeOf("$s12ValueGeneric4SlabV4makeQryF", 0) __<Element, N>
}

public protocol SlabProvider {
  associatedtype ProvidedSlab: Q
  associatedtype A where A == ProvidedSlab.A
  func provide() -> ProvidedSlab
}

// CHECK-LABEL: public struct GenericSlabProvider<let N : Swift::Int> : ValueGeneric::SlabProvider {
public struct GenericSlabProvider<let N: Int>: SlabProvider {
  public func provide() -> Slab<Int, N> { .init() }

  // CHECK: public typealias A = @_opaqueReturnTypeOf("$s12ValueGeneric4SlabV4makeQryF", 0) __<Swift::Int, N>
}

// CHECK-LABEL: public struct ConcreteSlabProvider : ValueGeneric::SlabProvider {
public struct ConcreteSlabProvider: SlabProvider {
  public func provide() -> Slab<Int, 2> { .init() }

  // CHECK: public typealias A = @_opaqueReturnTypeOf("$s12ValueGeneric4SlabV4makeQryF", 0) __<Swift::Int, 2>
}

// CHECK-LABEL: public struct NegativeSlabProvider : ValueGeneric::SlabProvider {
public struct NegativeSlabProvider: SlabProvider {
  public func provide() -> Slab<String, -10> { .init() }

  // CHECK: public typealias A = @_opaqueReturnTypeOf("$s12ValueGeneric4SlabV4makeQryF", 0) __<Swift::String, -10>
}

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
