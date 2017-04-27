// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_T011switch_objc13matchesEitherSbSo4HiveC5input_AD1aAD1btF :
func matchesEither(input: Hive, a: Hive, b: Hive) -> Bool {
  switch input {
  // CHECK:   function_ref @_T010ObjectiveC2teoiSbSo8NSObjectC_ADtF
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[RET_TRUE:bb[0-9]+]]
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_T010ObjectiveC2teoiSbSo8NSObjectC_ADtF
  // CHECK:   cond_br {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  case a, b:
  // CHECK:   function_ref @_T0S2b{{[_0-9a-zA-Z]*}}fC
    return true

  // CHECK: [[NOT_CASE2]]:
  // CHECK:   br [[RET_FALSE:bb[0-9]+]]
  default:
  // CHECK: [[RET_FALSE]]:
  // CHECK:   function_ref @_T0S2b{{[_0-9a-zA-Z]*}}fC
    return false
  }
}
