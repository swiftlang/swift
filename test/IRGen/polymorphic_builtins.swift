// This line tests that IRGen is properly turning the unspecialized builtins
// into traps.
//
// RUN: %target-swift-frontend  -enable-builtin-module -emit-ir -parse-as-library -Xllvm -sil-disable-pass=Simplification %s | %FileCheck %s

// Make sure we are not eliminating these builtins before IRGen runs. As part of
// the builtin's contract, we expect IRGen to convert them to traps, not
// anything before.
//
// RUN: %target-swift-frontend  -enable-builtin-module -Xllvm -sil-print-types -emit-sil -parse-as-library -Xllvm -sil-disable-pass=Simplification %s | %FileCheck --check-prefix=SIL %s

import Builtin

// SIL-LABEL: sil [transparent] @$s20polymorphic_builtins11_isConcrete4typeSbxm_tlF : $@convention(thin) <T> (@thick T.Type) -> Bool {
// SIL: builtin "isConcrete"<T>({{%[0-9]*}} : $@thick T.Type) : $Builtin.Int1
// SIL: // end sil function '$s20polymorphic_builtins11_isConcrete4typeSbxm_tlF'
@_transparent
public func _isConcrete<T>(type: T.Type) -> Bool {
  return Bool(_builtinBooleanLiteral: Builtin.isConcrete(type))
}

// SIL-LABEL: sil [transparent] @$s20polymorphic_builtins41calleeAddVectorsGenericTransparentGuardedyxx_xtlF : $@convention(thin) <T> (@in_guaranteed T, @in_guaranteed T) -> @out T {
// SIL: builtin "isConcrete"<T>({{%[0-9]*}} : $@thick T.Type) : $Builtin.Int1
// SIL: builtin "generic_add"<T>(
// SIL: } // end sil function '$s20polymorphic_builtins41calleeAddVectorsGenericTransparentGuardedyxx_xtlF'

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s20polymorphic_builtins41calleeAddVectorsGenericTransparentGuardedyxx_xtlF"(
// CHECK: br i1 false, label %[[CONCRETE_LABEL:[0-9][0-9]*]], label %[[NON_CONCRETE_LABEL:[0-9][0-9]*]]
//
// CHECK: [[CONCRETE_LABEL]]:
// CHECK:   call void @llvm.trap()
// CHECK:   br label %[[EPILOGUE_BLOCK:[0-9][0-9]*]]
//
// CHECK: [[NON_CONCRETE_LABEL]]
// CHECK:   br label %[[EPILOGUE_BLOCK]]
///
// CHECK: [[EPILOGUE_BLOCK]]:
// CHECK:   ret void
// CHECK-NEXT: }
@_transparent
public func calleeAddVectorsGenericTransparentGuarded<T>(_ lhs: T, _ rhs: T) -> T {
  if _isConcrete(T.self) {
    return Builtin.generic_add(lhs, rhs)
  }
  return lhs
}

public func callerAddVectorsGenericTransparent(_ lhs: Builtin.Vec4xInt32, _ rhs: Builtin.Vec4xInt32) -> Builtin.Vec4xInt32 {
  return calleeAddVectorsGenericTransparentGuarded(lhs, rhs)
}
