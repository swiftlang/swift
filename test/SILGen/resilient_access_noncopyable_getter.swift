// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/NCLib.swiftmodule -module-name=NCLib %S/Inputs/resilient_access_noncopyable_getter.swift -enable-experimental-feature Lifetimes
// RUN: %target-swift-emit-silgen %s -I %t -enable-experimental-feature Lifetimes | %FileCheck %s
// RUN: %target-swift-emit-sil %s -I %t -enable-experimental-feature Lifetimes -verify

import NCLib

// Verify that when accessing a noncopyable member on a resilient struct which explicitly declared a getter,
// we should be able to assign the result to a local variable as the getter returns an owned value.

// CHECK-LABEL: sil hidden [ossa] @$s35resilient_access_noncopyable_getter4testyyF : $@convention(thin) () -> ()
func test() {
  var source = Source()
  
  // CHECK: [[MUTSPAN_BOX:%.*]] = alloc_box {{.*}} var, name "mutSpan"
  // CHECK: [[MUTSPAN_BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[MUTSPAN_BOX]]
  // CHECK: [[MUTSPAN_ADDR:%.*]] = project_box [[MUTSPAN_BORROW]], 0
  // CHECK: [[MUTABLE_SPAN_GETTER:%.*]] = function_ref @$s5NCLib6SourceV11mutableSpans07MutableD0VySiGvg : $@convention(method) (@inout Source) -> @lifetime(borrow address_for_deps 0) @owned MutableSpan<Int>
  // CHECK: [[MUTSPAN:%.*]] = apply [[MUTABLE_SPAN_GETTER]]
  // CHECK: store [[MUTSPAN]] to [init] [[MUTSPAN_ADDR]]
  var mutSpan = source.mutableSpan
  mutSpan[0] = 1
}
