// RUN: %target-swift-emit-silgen %s -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeInheritance -disable-availability-checking | %FileCheck %s

// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

import SuperVirtualDispatch

extension Derived {
  public func callSuperVirtual() -> Int32 {
    return super.virtualMethod()
  }
}

extension LeafOverNoOverride {
  public func callSuperVirtualFromLeafOverNoOverride() -> Int32 {
    return super.virtualMethod()
  }
}

extension DerivedWithUnrelatedBase {
  public func callSuperVirtualFromMultiBase() -> Int32 {
    return super.virtualMethod()
  }
}

// CHECK-LABEL: Derived.callSuperVirtual()
// CHECK-NOT: super_method
// CHECK-NOT: objc_super_method
// CHECK-NOT: _ZNK7Derived
// CHECK-NOT: __synthesizedVirtualCall
// CHECK: function_ref {{.*}}Base{{.*}}__staticCall_virtualMethod
// CHECK: end sil function

// CHECK-LABEL: LeafOverNoOverride.callSuperVirtualFromLeafOverNoOverride()
// CHECK-NOT: super_method
// CHECK-NOT: objc_super_method
// CHECK-NOT: _ZNK18LeafOverNoOverride
// CHECK-NOT: __synthesizedVirtualCall
// CHECK: function_ref {{.*}}Base{{.*}}__staticCall_virtualMethod
// CHECK: end sil function
