// RUN: %target-swift-frontend -disable-availability-checking -emit-silgen -primary-file %s -primary-file %S/Inputs/opaque_result_type_inlinable_other.swift | %FileCheck %s
// RUN: %target-swift-frontend -disable-availability-checking -emit-silgen -primary-file %s %S/Inputs/opaque_result_type_inlinable_other.swift | %FileCheck %s
// RUN: %target-swift-frontend -disable-availability-checking -emit-silgen %s %S/Inputs/opaque_result_type_inlinable_other.swift | %FileCheck %s

// CHECK-LABEL: sil [serialized] [ossa] @$s28opaque_result_type_inlinable6callerQryF : $@convention(thin) () -> @out @_opaqueReturnTypeOf("$s28opaque_result_type_inlinable6callerQryF", 0) __ {
// CHECK: bb0(%0 : $*@_opaqueReturnTypeOf("$s28opaque_result_type_inlinable6calleeQryF", 0) __):
// CHECK: function_ref @$s28opaque_result_type_inlinable6calleeQryF : $@convention(thin) () -> @out @_opaqueReturnTypeOf("$s28opaque_result_type_inlinable6calleeQryF", 0) __

@inlinable public func caller() -> some Any {
  return callee()
}
