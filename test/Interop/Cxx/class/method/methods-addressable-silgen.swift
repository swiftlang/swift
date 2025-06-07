
// RUN: %target-swift-emit-silgen -I %S/Inputs -enable-experimental-feature AddressableParameters -enable-experimental-cxx-interop %s | %FileCheck %s

import Methods

// REQUIRES: swift_feature_AddressableParameters

public func addressableTest(x: borrowing @_addressable NonTrivialInWrapper, y: inout NonTrivialInWrapper) {
  let m = HasMethods()
  m.nonTrivialTakesConstRef(x)
  // No copy from the argument to the apply.
  // CHECK: addressableTest(x:y:)
  // CHECK: bb0([[INPUT:%[0-9]+]] : @noImplicitCopy $*NonTrivialInWrapper, [[INPUT2:%[0-9]+]] : $*NonTrivialInWrapper)
  // CHECK: [[WRAPPER:%[0-9]+]] = copyable_to_moveonlywrapper_addr [[INPUT]]
  // CHECK: [[MARKED:%[0-9]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[WRAPPER]]
  // CHECK: [[UNWRAPPED:%[0-9]+]] = moveonlywrapper_to_copyable_addr [[MARKED]]
  // CHECK: %{{[0-9]+}} = apply %{{[0-9]+}}([[UNWRAPPED]], %{{[0-9]+}}) : $@convention(cxx_method) (@in_guaranteed NonTrivialInWrapper, @in_guaranteed HasMethods) -> ()
  var m2 = HasMethods()
  // CHECK: [[ACCESS:%[0-9]+]] = begin_access [modify] [unknown] [[INPUT2]]
  // CHECK: %{{[0-9]+}} = apply %32([[ACCESS]], %{{[0-9]+}}) : $@convention(cxx_method) (@inout NonTrivialInWrapper, @in_guaranteed HasMethods) -> ()
  // CHECK-NEXT: end_access [[ACCESS]]
  m2.nonTrivialTakesRef(&y)
}
