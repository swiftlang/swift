// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/opaque_result_type_nested_optional_other.swift -emit-module-path %t/opaque_result_type_nested_optional_other.swiftmodule -disable-availability-checking
// RUN: %target-swift-emit-silgen %s -I %t | %FileCheck %s

import opaque_result_type_nested_optional_other

_ = bar(foo())

// CHECK-LABEL: sil [ossa] @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// CHECK: [[OUTER:%.*]] = alloc_stack $Optional<Int>
// CHECK: [[UNUSED:%.*]] = alloc_stack $S<Optional<Int>>
// CHECK: [[INNER:%.*]] = alloc_stack $S<Optional<Int>>
// CHECK: [[FN:%.*]] = function_ref @$s40opaque_result_type_nested_optional_other3bary1AQzxAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.A
// CHECK: apply [[FN]]<S<Optional<Int>>>([[OUTER]], [[INNER]]) : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.A
// CHECK: return
