// RUN: %target-swift-emit-silgen %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import DependentTypes

// Check the test function:
// CHECK-LABEL: sil [ossa] @$s4main4tests5Int64VyF : $@convention(thin) () -> Int
// CHECK:   [[ANY_OUT:%.*]] = alloc_stack $Any

// CHECK:   [[THUNK_REF:%.*]] = function_ref @$sSC27dependantReturnTypeInfferedyypSiF : $@convention(thin) (Int) -> @out Any
// CHECK:   apply [[THUNK_REF]]([[ANY_OUT]], %{{[0-9]+}}) : $@convention(thin) (Int) -> @out Any

// CHECK:   [[SPEC_OUT:%.*]] = alloc_stack $__CxxTemplateInst1MIxE
// CHECK:   unconditional_checked_cast_addr Any in [[ANY_OUT]] : $*Any to __CxxTemplateInst1MIxE in [[SPEC_OUT]] : $*__CxxTemplateInst1MIxE
// CHECK:   [[SPEC_VAL:%.*]] = load [trivial] [[SPEC_OUT]] : $*__CxxTemplateInst1MIxE

// CHECK:   [[SPEC_TEMP:%.*]] = alloc_stack $__CxxTemplateInst1MIxE
// CHECK:   store [[SPEC_VAL]] to [trivial] [[SPEC_TEMP]] : $*__CxxTemplateInst1MIxE

// CHECK:   [[GET_VAL_FN:%.*]] = function_ref @{{(\?getValue@\?\$M@_J@@QEBA_JXZ|_ZNK1MIlE8getValueEv)}} : $@convention(cxx_method) (@in_guaranteed __CxxTemplateInst1MIxE) -> Int
// CHECK:   [[OUT_VAL:%.*]] = apply [[GET_VAL_FN]]([[SPEC_TEMP]]) : $@convention(cxx_method) (@in_guaranteed __CxxTemplateInst1MIxE) -> Int

// CHECK:   return [[OUT_VAL]] : $Int
// CHECK-LABEL: end sil function '$s4main4tests5Int64VyF'


// Check the synthesized thunk:
// CHECK-LABEL: sil [transparent] [serialized] [ossa] @$sSC27dependantReturnTypeInfferedyypSiF : $@convention(thin) (Int) -> @out Any
// CHECK: bb0(%0 : $*Any, %1 : $Int):
// CHECK:   [[SPEC_OUT:%.*]] = alloc_stack $__CxxTemplateInst1MIxE
// CHECK:   [[TMP_INT:%.*]] = alloc_stack $Int
// CHECK:   store %1 to [trivial] [[TMP_INT]] : $*Int
// CHECK:   [[TMP_INT_CASTED:%.*]] = alloc_stack $Int
// CHECK:   unconditional_checked_cast_addr Int in [[TMP_INT]] : $*Int to Int in [[TMP_INT_CASTED]] : $*Int

// CHECK:  [[ARG:%.*]] = load [trivial] [[TMP_INT_CASTED]] : $*Int
// CHECK:   [[FN:%.*]] = function_ref @{{(\?\?\$dependantReturnTypeInffered@_J@@YA\?AU\?\$M@_J@@_J@Z|_Z27dependantReturnTypeInfferedIlE1MIT_ES1_)}} : $@convention(c) (Int) -> __CxxTemplateInst1MIxE
// CHECK:   [[OUT:%.*]] = apply [[FN]]([[ARG]]) : $@convention(c) (Int) -> __CxxTemplateInst1MIxE

// CHECK:   store [[OUT]] to [trivial] [[SPEC_OUT]] : $*__CxxTemplateInst1MIxE
// CHECK:   unconditional_checked_cast_addr __CxxTemplateInst1MIxE in [[SPEC_OUT]] : $*__CxxTemplateInst1MIxE to Any in %0 : $*Any
// CHECK-LABEL: end sil function '$sSC27dependantReturnTypeInfferedyypSiF'

public func test() -> Int64 {
  let m = dependantReturnTypeInffered(42) as! M<Int>
  return m.getValue()
}

// CHECK-LABEL: sil [clang __CxxTemplateInst1MIxE.getValue] @{{(\?getValue@\?\$M@_J@@QEBA_JXZ|_ZNK1MIlE8getValueEv)}} : $@convention(cxx_method) (@in_guaranteed __CxxTemplateInst1MIxE) -> Int64
// CHECK-LABEL: sil [serializable] [clang dependantReturnTypeInffered] @{{(\?\?\$dependantReturnTypeInffered@_J@@YA\?AU\?\$M@_J@@_J@Z|_Z27dependantReturnTypeInfferedIlE1MIT_ES1_)}} : $@convention(c) (Int) -> __CxxTemplateInst1MIxE
