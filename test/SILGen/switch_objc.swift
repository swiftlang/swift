// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden [ossa] @$s11switch_objc13matchesEither5input1a1bSbSo4HiveC_A2GtF :
func matchesEither(input: Hive, a: Hive, b: Hive) -> Bool {
  switch input {
  // CHECK:   function_ref @$s10ObjectiveC2teoiySbSo8NSObjectC_ADtF
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[RET_TRUE:bb[0-9]+]]
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @$s10ObjectiveC2teoiySbSo8NSObjectC_ADtF
  // CHECK:   cond_br {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  case a, b:
  // CHECK:   function_ref @$sSb2{{[_0-9a-zA-Z]*}}fC
    return true

  // CHECK: [[NOT_CASE2]]:
  default:
  // CHECK:   function_ref @$sSb2{{[_0-9a-zA-Z]*}}fC
    return false
  }
}

@objc enum ObjCEnum : UInt8 {
case first
case second
}

// CHECK-LABEL: sil hidden [ossa] @$s11switch_objc44checkObjCEnumUnhandledCaseDiagnosticEmission1xyAA0dE0O_tF : $@convention(thin) (ObjCEnum) -> () {
// CHECK: bb0([[ARG:%.*]] :
// CHECK:   [[METATYPE:%.*]] = metatype $@thick ObjCEnum.Type
// CHECK:   [[ARG_INT_REPR:%.*]] = unchecked_trivial_bit_cast [[ARG]]
// CHECK:   switch_enum [[ARG]] : $ObjCEnum, {{.*}}, default [[DEFAULT_BB:bb[0-9]+]]
//
// CHECK: [[DEFAULT_BB]](
// CHECK:   [[STACK_SLOT:%.*]] = alloc_stack $UInt8
// CHECK:   store [[ARG_INT_REPR]] to [trivial] [[STACK_SLOT]]
// CHECK:   [[DIAGNOSE_FUNC:%.*]] = function_ref @$ss32_diagnoseUnexpectedEnumCaseValue4type03rawE0s5NeverOxm_q_tr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@thick τ_0_0.Type, @in_guaranteed τ_0_1) -> Never
// CHECK:   apply [[DIAGNOSE_FUNC]]<ObjCEnum, UInt8>([[METATYPE]], [[STACK_SLOT]])
// CHECK:   unreachable
// CHECK: } // end sil function '$s11switch_objc44checkObjCEnumUnhandledCaseDiagnosticEmission1xyAA0dE0O_tF'
func checkObjCEnumUnhandledCaseDiagnosticEmission(x: ObjCEnum) {
  switch x {
  case .first:
    break
  case .second:
    break
  }
}
