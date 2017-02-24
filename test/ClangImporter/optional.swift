// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xllvm -new-mangling-for-tests -I %S/Inputs/custom-modules -emit-silgen -o - %s | %FileCheck %s

// REQUIRES: objc_interop

import ObjectiveC
import Foundation
import objc_ext
import TestProtocols

class A {
  @objc func foo() -> String? {
    return ""
  }
// CHECK-LABEL:    sil hidden [thunk] @_T08optional1AC3fooSSSgyFTo : $@convention(objc_method) (A) -> @autoreleased Optional<NSString>
// CHECK:    bb0([[SELF:%.*]] : $A):
// CHECK:      [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:      [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
// CHECK:      [[T0:%.*]] = function_ref @_T08optional1AC3fooSSSgyF
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[BORROWED_SELF_COPY]])
// CHECK-NEXT: end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
// CHECK-NEXT: destroy_value [[SELF_COPY]]
// CHECK:      [[T2:%.*]] = select_enum [[T1]]
// CHECK-NEXT: cond_br [[T2]]
//   Something branch: project value, translate, inject into result.
// CHECK:      [[STR:%.*]] = unchecked_enum_data [[T1]]
// CHECK:      [[T0:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK-NEXT: [[BORROWED_STR:%.*]] = begin_borrow [[STR]]
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[BORROWED_STR]])
// CHECK-NEXT: enum $Optional<NSString>, #Optional.some!enumelt.1, [[T1]]
// CHECK-NEXT: end_borrow [[BORROWED_STR:%.*]] from [[STR]]
// CHECK-NEXT: destroy_value [[STR]]
// CHECK-NEXT: br
//   Nothing branch: inject nothing into result.
// CHECK:      enum $Optional<NSString>, #Optional.none!enumelt
// CHECK-NEXT: br
//   Continuation.
// CHECK:      bb3([[T0:%.*]] : $Optional<NSString>):
// CHECK-NEXT: return [[T0]]

  @objc func bar(x x : String?) {}
// CHECK-LABEL:    sil hidden [thunk] @_T08optional1AC3barySSSg1x_tFTo : $@convention(objc_method) (Optional<NSString>, A) -> ()
// CHECK:    bb0([[ARG:%.*]] : $Optional<NSString>, [[SELF:%.*]] : $A):
// CHECK:      [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:      [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:      [[T1:%.*]] = select_enum [[ARG_COPY]]
// CHECK-NEXT: cond_br [[T1]]
//   Something branch: project value, translate, inject into result.
// CHECK:      [[NSSTR:%.*]] = unchecked_enum_data [[ARG_COPY]]
// CHECK:      [[T0:%.*]] = function_ref @_T0SS10FoundationE36_unconditionallyBridgeFromObjectiveCSSSo8NSStringCSgFZ
//   Make a temporary initialized string that we're going to clobber as part of the conversion process (?).
// CHECK-NEXT: [[NSSTR_BOX:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[NSSTR]] : $NSString
// CHECK-NEXT: [[STRING_META:%.*]] = metatype $@thin String.Type
// CHECK-NEXT: [[T1:%.*]] = apply [[T0]]([[NSSTR_BOX]], [[STRING_META]])
// CHECK-NEXT: enum $Optional<String>, #Optional.some!enumelt.1, [[T1]]
// CHECK-NEXT: br
//   Nothing branch: inject nothing into result.
// CHECK:      enum $Optional<String>, #Optional.none!enumelt
// CHECK-NEXT: br
//   Continuation.
// CHECK:      bb3([[T0:%.*]] : $Optional<String>):
// CHECK:      [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
// CHECK:      [[T1:%.*]] = function_ref @_T08optional1AC3barySSSg1x_tF
// CHECK-NEXT: [[T2:%.*]] = apply [[T1]]([[T0]], [[BORROWED_SELF_COPY]])
// CHECK-NEXT: end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
// CHECK-NEXT: destroy_value [[SELF_COPY]]
// CHECK-NEXT: return [[T2]] : $()
}


// rdar://15144951
class TestWeak : NSObject {
  weak var b : WeakObject? = nil
}
class WeakObject : NSObject {}
