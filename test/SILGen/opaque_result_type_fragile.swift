// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -disable-availability-checking -emit-module %S/Inputs/opaque_result_type_fragile_other.swift -emit-module-path %t/opaque_result_type_fragile_other.swiftmodule
// RUN: %target-swift-frontend -disable-availability-checking -emit-silgen -I%t %s | %FileCheck %s

import opaque_result_type_fragile_other

struct InternalView: View {}
public struct PublicView: View {}

public func testInternalView() {
  let v = InternalView()

  // CHECK: function_ref @$s32opaque_result_type_fragile_other4ViewPAAE11passThroughQryF : $@convention(method) <τ_0_0 where τ_0_0 : View> (@in_guaranteed τ_0_0) -> @out τ_0_0
  _ = v.passThrough()

  // CHECK: function_ref @$s32opaque_result_type_fragile_other4ViewPAAE016wrapWithInternalF0QryF : $@convention(method) <τ_0_0 where τ_0_0 : View> (@in_guaranteed τ_0_0) -> @out @_opaqueReturnTypeOf("$s32opaque_result_type_fragile_other4ViewPAAE016wrapWithInternalF0QryF", 0) 🦸<τ_0_0>
  _ = v.wrapWithInternalView()

  // CHECK: function_ref @$s32opaque_result_type_fragile_other4ViewPAAE023wrapWithInternalGenericF0QryF : $@convention(method) <τ_0_0 where τ_0_0 : View> (@in_guaranteed τ_0_0) -> @out @_opaqueReturnTypeOf("$s32opaque_result_type_fragile_other4ViewPAAE023wrapWithInternalGenericF0QryF", 0) 🦸<τ_0_0>
  _ = v.wrapWithInternalGenericView()

  // CHECK: function_ref @$s32opaque_result_type_fragile_other4ViewPAAE014wrapWithPublicF0QryF : $@convention(method) <τ_0_0 where τ_0_0 : View> (@in_guaranteed τ_0_0) -> @out PublicView
  _ = v.wrapWithPublicView()

  //CHECK: function_ref @$s32opaque_result_type_fragile_other4ViewPAAE021wrapWithPublicGenericF0QryF : $@convention(method) <τ_0_0 where τ_0_0 : View> (@in_guaranteed τ_0_0) -> @out PublicGenericView<τ_0_0>
  _ = v.wrapWithPublicGenericView()
}

public func testPublicView() {
  let v = PublicView()

  // CHECK: function_ref @$s32opaque_result_type_fragile_other4ViewPAAE11passThroughQryF : $@convention(method) <τ_0_0 where τ_0_0 : View> (@in_guaranteed τ_0_0) -> @out τ_0_0
  _ = v.passThrough()

  // CHECK: function_ref @$s32opaque_result_type_fragile_other4ViewPAAE016wrapWithInternalF0QryF : $@convention(method) <τ_0_0 where τ_0_0 : View> (@in_guaranteed τ_0_0) -> @out @_opaqueReturnTypeOf("$s32opaque_result_type_fragile_other4ViewPAAE016wrapWithInternalF0QryF", 0) 🦸<τ_0_0>
  _ = v.wrapWithInternalView()

  // CHECK: function_ref @$s32opaque_result_type_fragile_other4ViewPAAE023wrapWithInternalGenericF0QryF : $@convention(method) <τ_0_0 where τ_0_0 : View> (@in_guaranteed τ_0_0) -> @out @_opaqueReturnTypeOf("$s32opaque_result_type_fragile_other4ViewPAAE023wrapWithInternalGenericF0QryF", 0) 🦸<τ_0_0>
  _ = v.wrapWithInternalGenericView()

  // CHECK: function_ref @$s32opaque_result_type_fragile_other4ViewPAAE014wrapWithPublicF0QryF : $@convention(method) <τ_0_0 where τ_0_0 : View> (@in_guaranteed τ_0_0) -> @out PublicView
  _ = v.wrapWithPublicView()

  //CHECK: function_ref @$s32opaque_result_type_fragile_other4ViewPAAE021wrapWithPublicGenericF0QryF : $@convention(method) <τ_0_0 where τ_0_0 : View> (@in_guaranteed τ_0_0) -> @out PublicGenericView<τ_0_0>
  _ = v.wrapWithPublicGenericView()
}