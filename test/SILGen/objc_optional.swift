// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -module-name objc_bridged_results %s -import-objc-header %S/Inputs/objc_optional.h | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden [ossa] @$s20objc_bridged_results20testOptionalShortCutyySo4TestC_ADtF : $@convention(thin) (@guaranteed Test, @guaranteed Test) -> () {
// CHECK: bb0(%0 : @guaranteed $Test, %1 : @guaranteed $Test):
// CHECK:   [[GETTER:%.*]] = objc_method %1 : $Test, #Test.other!getter.1.foreign
// CHECK:   [[RES:%.*]] = apply [[GETTER]](%1)
// CHECK:   [[SETTER:%.*]] = objc_method %0 : $Test, #Test.other!setter.1.foreign
// CHECK:   apply [[SETTER]]([[RES]], %0)
// CHECK:   destroy_value [[RES]]
// CHECK:   return
func testOptionalShortCut(_ t: Test, _ v: Test) {
  t.other = v.other
}

// Bridged things are not shortcut.
// CHECK-LABEL: sil hidden [ossa] @$s20objc_bridged_results10noShortCutyySo4TestC_ADtF
// CHECK: switch_enum
// CHECK: switch_enum
// CHECK: return
func noShortCut(_ t: Test, _ v: Test) {
  t.nullableString = v.nullableString
}

class InClass {
  var t: Test?
  var v: Test?

// CHECK-LABEL: sil hidden [ossa] @$s20objc_bridged_results7InClassC20testOptionalShortCutyyF : $@convention(method) (@guaranteed InClass) -> () {
// CHECK: bb0(%0 : @guaranteed $InClass):
// CHECK:   [[TMP_T:%.*]] = alloc_stack $Optional<Test>
// CHECK:   [[GET_T:%.*]] = class_method %0 : $InClass, #InClass.t!getter.1
// CHECK:   [[T:%.*]] = apply [[GET_T]](%0)
// CHECK:   store [[T]] to [init] [[TMP_T]] : $*Optional<Test>
// CHECK:   [[TMP_T2:%.*]] = unchecked_addr_cast [[TMP_T]] : $*Optional<Test> to $*Test
// CHECK:   [[T2:%.*]] = load [copy] [[TMP_T2]] : $*Test
// CHECK:   destroy_addr [[TMP_T]] : $*Optional<Test>
// CHECK:   [[TMP_V:%.*]] = alloc_stack $Optional<Test>
// CHECK:   [[GET_V:%.*]] = class_method %0 : $InClass, #InClass.v!getter.1
// CHECK:   [[V:%.*]] = apply [[GET_V]](%0)
// CHECK:   store [[V]] to [init] [[TMP_V]] : $*Optional<Test>
// CHECK:   [[TMP_V2:%.*]] = unchecked_addr_cast [[TMP_V]] : $*Optional<Test> to $*Test
// CHECK:   [[V2:%.*]] = load [copy] [[TMP_V2]] : $*Test
// CHECK:   destroy_addr [[TMP_V]] : $*Optional<Test>
// CHECK:   [[V2_B:%.*]] = begin_borrow [[V2]] : $Test
// CHECK:   [[GET_OTHER:%.*]] = objc_method [[V2_B]] : $Test, #Test.other!getter.1.foreign
// CHECK:   [[VOTHER:%.*]] = apply [[GET_OTHER]]([[V2_B]])
// CHECK:   end_borrow [[V2_B]] : $Test
// CHECK:   destroy_value [[V2]] : $Test
// CHECK:   dealloc_stack [[TMP_V]] : $*Optional<Test>
// CHECK:   [[SET_OTHER:%.*]] = objc_method [[T2]] : $Test, #Test.other!setter.1.foreign
// CHECK:   apply [[SET_OTHER]]([[VOTHER]], [[T2]])
// CHECK:   destroy_value [[VOTHER]] : $Optional<Test>
// CHECK:   destroy_value [[T2]] : $Test
// CHECK:   dealloc_stack [[TMP_T]] : $*Optional<Test>
// CHECK:   return
// CHECK: }
  func testOptionalShortCut() {
    t?.other = v?.other
  }
// CHECK-LABEL: sil hidden [ossa] @$s20objc_bridged_results7InClassC21testOptionalShortCut2yyF
// CHECK-NOT: switch_enum
// CHECK-NOT: select_enum
// CHECK return
  func testOptionalShortCut2() {
    t?.other?.other = v?.other?.other
  }

// CHECK-LABEL: sil hidden [ossa] @$s20objc_bridged_results7InClassC14testVoidReturnyyF
// CHECK:  [[TMP_T:%.*]] = alloc_stack $Optional<Test>
// CHECK:  [[GETTER:%.*]] = class_method %0 : $InClass, #InClass.t!getter.1
// CHECK:  [[T:%.*]] = apply [[GETTER]](%0)
// CHECK:  store [[T]] to [init] [[TMP_T]] : $*Optional<Test>
// CHECK:  [[T2_ADDR:%.*]] = unchecked_addr_cast [[TMP_T]] : $*Optional<Test> to $*Test
// CHECK:  [[T2:%.*]] = load [copy] [[T2_ADDR]] : $*Test
// CHECK:  destroy_addr [[TMP_T]] : $*Optional<Test>
// CHECK:  [[RETVOID:%.*]] = objc_method [[T2]] : $Test, #Test.returnVoid!1.foreign
// CHECK:  apply [[RETVOID]]([[T2]])
// CHECK:  destroy_value [[T2]] : $Test
// CHECK:  dealloc_stack [[TMP_T]]
// CHECK:  return
  func testVoidReturn() {
    t?.returnVoid()
  }
}

