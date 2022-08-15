// RUN: %target-swift-emit-silgen %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import DependentTypes

// Check the test function:
// CHECK-LABEL: sil [ossa] @$s4main4tests5Int64VyF : $@convention(thin) () -> Int64
// CHECK:   [[ANY_OUT:%.*]] = alloc_stack $Any

// CHECK:   [[THUNK_REF:%.*]] = function_ref @$sSC27dependantReturnTypeInferredyyps5Int64VF : $@convention(thin) (Int64) -> @out Any
// CHECK:   apply [[THUNK_REF]]([[ANY_OUT]], %{{[0-9]+}}) : $@convention(thin) (Int64) -> @out Any

// CHECK:   [[SPEC_OUT:%.*]] = alloc_stack $__CxxTemplateInst1MIxE
// CHECK:   unconditional_checked_cast_addr Any in [[ANY_OUT]] : $*Any to __CxxTemplateInst1MIxE in [[SPEC_OUT]] : $*__CxxTemplateInst1MIxE
// CHECK:   [[SPEC_VAL:%.*]] = load [trivial] [[SPEC_OUT]] : $*__CxxTemplateInst1MIxE

// CHECK:   [[SPEC_TEMP:%.*]] = alloc_stack $__CxxTemplateInst1MIxE
// CHECK:   store [[SPEC_VAL]] to [trivial] [[SPEC_TEMP]] : $*__CxxTemplateInst1MIxE

// CHECK:   [[GET_VAL_FN:%.*]] = function_ref @{{_ZNK1MIxE8getValueEv|\?getValue@\?\$M@_J@@QEBA_JXZ}} : $@convention(cxx_method) (@in_guaranteed __CxxTemplateInst1MIxE) -> Int
// CHECK:   [[OUT_VAL:%.*]] = apply [[GET_VAL_FN]]([[SPEC_TEMP]]) : $@convention(cxx_method) (@in_guaranteed __CxxTemplateInst1MIxE) -> Int

// CHECK:   return [[OUT_VAL]] : $Int
// CHECK-LABEL: end sil function '$s4main4tests5Int64VyF'


// Check the synthesized thunk:
// CHECK-LABEL: sil [transparent] [serialized] [ossa] @$sSC27dependantReturnTypeInferredyyps5Int64VF : $@convention(thin) (Int64) -> @out Any
// CHECK: bb0(%0 : $*Any, %1 : $Int64):
// CHECK:   [[SPEC_OUT:%.*]] = alloc_stack $__CxxTemplateInst1MIxE

// CHECK:   [[FN:%.*]] = function_ref @{{_Z27dependantReturnTypeInferredIxE1MIT_ES1_|\?\?\$dependantReturnTypeInferred@_J@@YA\?AU\?\$M@_J@@_J@Z}} : $@convention(c) (Int64) -> __CxxTemplateInst1MIxE
// CHECK:   [[OUT:%.*]] = apply [[FN]](%1) : $@convention(c) (Int64) -> __CxxTemplateInst1MIxE

// CHECK:   store [[OUT]] to [trivial] [[SPEC_OUT]] : $*__CxxTemplateInst1MIxE
// CHECK:   unconditional_checked_cast_addr __CxxTemplateInst1MIxE in [[SPEC_OUT]] : $*__CxxTemplateInst1MIxE to Any in %0 : $*Any
// CHECK-LABEL: end sil function '$sSC27dependantReturnTypeInferredyyps5Int64VF'

public func test() -> Int64 {
  let m = dependantReturnTypeInferred(Int64(42)) as! M<Int64>
  return m.getValue()
}

// CHECK-LABEL: sil [clang __CxxTemplateInst1MIxE.getValue] @{{_ZNK1MIxE8getValueEv|\?getValue@\?\$M@_J@@QEBA_JXZ}} : $@convention(cxx_method) (@in_guaranteed __CxxTemplateInst1MIxE) -> Int64
// CHECK-LABEL: sil [serialized] [clang dependantReturnTypeInferred]  @{{_Z27dependantReturnTypeInferredIxE1MIT_ES1_|\?\?\$dependantReturnTypeInferred@_J@@YA\?AU\?\$M@_J@@_J@Z}} : $@convention(c) (Int64) -> __CxxTemplateInst1MIxE
