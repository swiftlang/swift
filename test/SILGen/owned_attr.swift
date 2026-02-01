// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/OwnedAttrLib.swiftmodule -emit-module-interface-path %t/OwnedAttrLib.swiftinterface -module-name=OwnedAttrLib %S/Inputs/owned_attr.swift \
// RUN:     -enable-experimental-feature Lifetimes

// RUN: %target-swift-emit-silgen -module-name X %s -I %t -enable-experimental-feature Lifetimes | %FileCheck %s
// RUN: %target-swift-emit-sil -module-name X %s -I %t -enable-experimental-feature Lifetimes -verify

// RUN: %FileCheck --check-prefix CHECK-INTERFACE %s < %t/OwnedAttrLib.swiftinterface

// REQUIRES: swift_feature_Lifetimes

import OwnedAttrLib

// CHECK-INTERFACE: public struct Source
// CHECK-INTERFACE: #if compiler(>=5.3) {{.*}}&& $UnderscoreOwned
// CHECK-INTERFACE: @_owned public var mutableSpan

// CHECK-INTERFACE: public protocol Giver
// CHECK-INTERFACE: #if compiler(>=5.3) {{.*}}&& $UnderscoreOwned
// CHECK-INTERFACE: @_owned var mutableSpan{{.*}} { mutating get }

// Verify that when accessing a property of noncopyable type within a resilient struct, where that property
// has an @_owned getter, we get an owned value.

// CHECK-LABEL: sil hidden [ossa] @$s1X4testyyF : $@convention(thin) () -> ()
func test() {
  var source = Source()
  
  // CHECK: [[MUTSPAN_BOX:%.*]] = alloc_box {{.*}} var, name "mutSpan"
  // CHECK: [[MUTSPAN_BORROW:%.*]] = begin_borrow [lexical] [var_decl] [[MUTSPAN_BOX]]
  // CHECK: [[MUTSPAN_ADDR:%.*]] = project_box [[MUTSPAN_BORROW]], 0
  // CHECK: [[MUTABLE_SPAN_GETTER:%.*]] = function_ref @$s12OwnedAttrLib6SourceV11mutableSpans07MutableF0VySiGvg : $@convention(method) (@inout Source) -> @lifetime(borrow address_for_deps 0) @owned MutableSpan<Int>
  // CHECK: [[MUTSPAN:%.*]] = apply [[MUTABLE_SPAN_GETTER]]
  // CHECK: store [[MUTSPAN]] to [init] [[MUTSPAN_ADDR]]
  var mutSpan = source.mutableSpan
  mutSpan[0] = 1
}

// CHECK-LABEL: sil hidden [ossa] @$s1X5test2yyx12OwnedAttrLib5GiverRzlF
func test2(_ s: some Giver) {
  var source = s
  // CHECK: witness_method $τ_0_0, #Giver.mutableSpan!getter : <Self where Self : OwnedAttrLib.Giver, Self : ~Copyable> (inout Self) -> @lifetime(borrow 0) () -> MutableSpan<Int> : $@convention(witness_method: Giver) <τ_0_0 where τ_0_0 : Giver, τ_0_0 : ~Copyable> (@inout τ_0_0) -> @lifetime(borrow address_for_deps 0) @owned MutableSpan<Int>
  // CHECK: apply {{.*}} : $@convention(witness_method: Giver) {{.*}} -> @lifetime(borrow address_for_deps 0) @owned MutableSpan<Int>
  // CHECK-NOT: witness_method
  var mutSpan = source.mutableSpan
  mutSpan[0] = 1
}
