
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name optional -I %S/Inputs/custom-modules -enable-sil-ownership -emit-silgen -o - %s | %FileCheck %s

// REQUIRES: objc_interop

import ObjectiveC
import Foundation
import objc_ext
import TestProtocols

class A {
  @objc func foo() -> String? {
    return ""
  }
// CHECK-LABEL:    sil hidden [thunk] @$s8optional1AC3fooSSSgyFTo : $@convention(objc_method) (A) -> @autoreleased Optional<NSString>
// CHECK:    bb0([[SELF:%.*]] : @unowned $A):
// CHECK:      [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:      [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
// CHECK:      [[T0:%.*]] = function_ref @$s8optional1AC3fooSSSgyF
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[BORROWED_SELF_COPY]])
// CHECK-NEXT: end_borrow [[BORROWED_SELF_COPY]]
// CHECK-NEXT: destroy_value [[SELF_COPY]]
// CHECK-NEXT: switch_enum [[T1]] : $Optional<String>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
//   Something branch: project value, translate, inject into result.
// CHECK:    [[SOME_BB]]([[STR:%.*]] : @owned $String):
// CHECK:      [[T0:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK-NEXT: [[BORROWED_STR:%.*]] = begin_borrow [[STR]]
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[BORROWED_STR]])
// CHECK-NEXT: enum $Optional<NSString>, #Optional.some!enumelt.1, [[T1]]
// CHECK-NEXT: end_borrow [[BORROWED_STR:%.*]]
// CHECK-NEXT: destroy_value [[STR]]
// CHECK-NEXT: br
//   Nothing branch: inject nothing into result.
//
// CHECK: [[NONE_BB]]:
// CHECK-NEXT: enum $Optional<NSString>, #Optional.none!enumelt
// CHECK-NEXT: br
//   Continuation.
// CHECK: bb3([[T0:%.*]] : @owned $Optional<NSString>):
// CHECK-NEXT: return [[T0]]

  @objc func bar(x x : String?) {}
// CHECK-LABEL:    sil hidden [thunk] @$s8optional1AC3bar1xySSSg_tFTo : $@convention(objc_method) (Optional<NSString>, A) -> ()
// CHECK:    bb0([[ARG:%.*]] : @unowned $Optional<NSString>, [[SELF:%.*]] : @unowned $A):
// CHECK:      [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:      [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:      switch_enum [[ARG_COPY]] : $Optional<NSString>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
//   Something branch: project value, translate, inject into result.
// CHECK:    [[SOME_BB]]([[NSSTR:%.*]] : @owned $NSString):
// CHECK:      [[T0:%.*]] = function_ref @$sSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
//   Make a temporary initialized string that we're going to clobber as part of the conversion process (?).
// CHECK-NEXT: [[NSSTR_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[NSSTR]] : $NSString
// CHECK-NEXT: [[STRING_META:%.*]] = metatype $@thin String.Type
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[NSSTR_BOX]], [[STRING_META]])
// CHECK-NEXT: enum $Optional<String>, #Optional.some!enumelt.1, [[T1]]
// CHECK-NEXT: destroy_value [[NSSTR_BOX]]
// CHECK-NEXT: br
//
//   Nothing branch: inject nothing into result.
// CHECK:   [[NONE_BB]]:
// CHECK:      enum $Optional<String>, #Optional.none!enumelt
// CHECK-NEXT: br
//   Continuation.
// CHECK:      bb3([[T0:%.*]] : @owned $Optional<String>):
// CHECK:      [[BORROWED_T0:%.*]] = begin_borrow [[T0]]
// CHECK:      [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
// CHECK:      [[T1:%.*]] = function_ref @$s8optional1AC3bar1xySSSg_tF
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[BORROWED_T0]], [[BORROWED_SELF_COPY]])
// CHECK-NEXT: end_borrow [[BORROWED_SELF_COPY]]
// CHECK-NEXT: end_borrow [[BORROWED_T0]]
// CHECK-NEXT: destroy_value [[T0]]
// CHECK-NEXT: destroy_value [[SELF_COPY]]
// CHECK-NEXT: return [[T2]] : $()
}


// rdar://15144951
class TestWeak : NSObject {
  weak var b : WeakObject? = nil
}
class WeakObject : NSObject {}
