// RUN: %target-swift-frontend -enable-library-evolution -disable-availability-checking -O -Xllvm -sil-print-types -emit-sil %s %S/Inputs/cast_folding_opaque_other.swift -module-name cast_folding_opaque
// RUN: %target-swift-frontend -enable-library-evolution -disable-availability-checking -Onone -Xllvm -sil-print-types -emit-sil -primary-file %s %S/Inputs/cast_folding_opaque_other.swift -module-name cast_folding_opaque | %FileCheck %s

// CHECK-LABEL: sil @$s19cast_folding_opaque23testCastOpaqueArchetypeAA10UnderlyingVyF
// CHECK:   [[O:%.*]] = alloc_stack $@_opaqueReturnTypeOf("$s19cast_folding_opaque12returnOpaqueQryF", 0)
// CHECK:   [[F:%.*]] = function_ref @$s19cast_folding_opaque12returnOpaqueQryF
// CHECK:   apply [[F]]([[O]])
// CHECK:   unconditional_checked_cast_addr @_opaqueReturnTypeOf{{.*}}in [[O]] : $*@_opaqueReturnTypeOf{{.*}}to Underlying in %0 : $*Underlying
@inlinable
public func testCastOpaqueArchetype() -> Underlying {
  return returnOpaque() as! Underlying
}
