// RUN: %target-swift-frontend -typecheck -disable-availability-checking -dump-ast -enable-experimental-opaque-type-erasure %s | %FileCheck %s

public struct AnyP: P {
  public init<T: P>(erasing: T) {}
}

@_typeEraser(AnyP)
public protocol P {}

public struct ConcreteP: P, Hashable {
  public init() {}
}

// CHECK-LABEL: testTypeErased
@inlinable public func testTypeErased() -> some P {
  // CHECK: underlying_to_opaque_expr{{.*}}"some P"
  // CHECK-NEXT: call_expr implicit type="AnyP"
  ConcreteP()
}

// Check with -enable-experimental-opaque-type-erasure

// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/test1/erasure.swiftinterface) %s -module-name erasure -enable-experimental-opaque-type-erasure -enable-library-evolution -disable-availability-checking
// RUN: %FileCheck %s --check-prefix CHECK-INTERFACE < %t/test1/erasure.swiftinterface
// CHECK-INTERFACE: swift-module-flags:{{.*}} -enable-experimental-opaque-type-erasure

// RUN: %target-swift-frontend -disable-availability-checking -I %t/test1/ -emit-sil %S/Inputs/import_with_opaque_type_erasure.swift | %FileCheck %s --check-prefix CHECK-UNDERLYING-TYPE
// CHECK-UNDERLYING-TYPE-LABEL: s31import_with_opaque_type_erasure6erasedQrvg
// CHECK-UNDERLYING-TYPE: bb0(%0 : $*AnyP):
// CHECK-UNDERLYING-TYPE: function_ref @$s7erasure14testTypeErasedQryF : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <AnyP>


// Check with -enable-experimental-feature OpaqueTypeErasure

// RUN: %target-swift-emit-module-interface(%t/test2/erasure.swiftinterface) %s -module-name erasure -enable-experimental-feature OpaqueTypeErasure -enable-library-evolution -disable-availability-checking
// RUN: %FileCheck %s --check-prefix CHECK-INTERFACE2 < %t/test2/erasure.swiftinterface
// CHECK-INTERFACE2: swift-module-flags:{{.*}} -enable-experimental-feature OpaqueTypeErasure

// RUN: %target-swift-frontend -disable-availability-checking -I %t/test2/ -emit-sil %S/Inputs/import_with_opaque_type_erasure.swift | %FileCheck %s --check-prefix CHECK-UNDERLYING-TYPE2

// REQUIRES: swift_feature_OpaqueTypeErasure
// CHECK-UNDERLYING-TYPE2-LABEL: s31import_with_opaque_type_erasure6erasedQrvg
// CHECK-UNDERLYING-TYPE2: bb0(%0 : $*AnyP):
// CHECK-UNDERLYING-TYPE2: function_ref @$s7erasure14testTypeErasedQryF : $@convention(thin) @substituted <τ_0_0> () -> @out τ_0_0 for <AnyP>
