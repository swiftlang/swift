// RUN: %target-swift-emit-sil %s -enable-experimental-feature Embedded -wmo -module-name test | %FileCheck %s

// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// If an associated type is an opaque result type, the associated conformance is
// abstract. Witness table specialization in the mandatory pipeline must not treat such
// a conformance as concrete.

public protocol Q {
  func g()
}

struct SomeQ: Q {
  func g() {}
}

public protocol P: AnyObject {
  associatedtype A: Q
  func make() -> A
}

public protocol P2: P {}

public final class GenericClass<T>: P2 {
  public func make() -> some Q { SomeQ() }
}

public final class NonGenericClass: P2 {
  public func make() -> some Q { SomeQ() }
}

public func testGeneric() -> any P2 {
  return GenericClass<Int>()
}

public func testNonGeneric() -> any P2 {
  return NonGenericClass()
}

// The specialized witness table is created and keeps the abstract associated conformance
// of the opaque type. IRGen resolves it to the underlying type's conformance.

// CHECK-LABEL: sil_witness_table shared [specialized] GenericClass<Int>: specialize <Int> (<T> GenericClass<T>: P module test) {
// CHECK-NEXT:    associated_conformance (A: Q): dependent @_opaqueReturnTypeOf("$e4test12GenericClassC4makeQryF", 0) __<Int>
// CHECK-NEXT:    associated_type A: @_opaqueReturnTypeOf("$e4test12GenericClassC4makeQryF", 0) __<Int>
// CHECK-NEXT:    method #P.make{{.*}}: @$e4test12GenericClassCyxGAA1PA2aEP4make1AQzyFTWSi_Tgq5
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table shared [specialized] GenericClass<Int>: specialize <Int> (<T> GenericClass<T>: P2 module test) {
// CHECK-NEXT:    base_protocol P: GenericClass<Int>: specialize <Int> (<T> GenericClass<T>: P module test)
// CHECK-NEXT:  }
