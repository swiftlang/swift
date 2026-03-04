// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -import-objc-header %S/Inputs/objc_sending.h %s | %FileCheck %s

// REQUIRES: objc_interop

// Regression test for S623392: double-free when ObjC calls an @objc thunk
// for a method with a `sending` parameter.
//
// Root cause: the type checker promotes `sending` → ImplicitlyCopyableConsuming
// → ValueOwnership::Owned → Direct_Owned convention. But ObjC doesn't know
// about `sending` and always delivers at +0. The thunk saw @owned, skipped the
// retain, and forwarded +0 to the Swift entry point expecting +1 → double-free.
//
// Fix (SILFunctionType.cpp): override Direct_Owned → Direct_Unowned for ObjC
// methods with `sending` parameters. The thunk parameter becomes @unowned, so
// emitObjCUnconsumedArgument emits a copy_value (retain) before forwarding.

// ============================================================================
// Case 1: Swift class conforming to ObjC protocol with `sending` parameter.
// This exercises ObjCMethodConventions (the Clang declaration exists).
// ============================================================================

class Handler: NSObject, ObjCHandlerWithSending {
  func handle(_ value: sending NSObject) {}
}

// The native Swift entry point expects @sil_sending @owned.
// CHECK-LABEL: sil hidden [ossa] @$s12objc_sending7HandlerC6handleyySo8NSObjectCnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Handler) -> ()

// The @objc thunk must receive the sending param as @unowned and retain it.
//
// Before the fix, the thunk was:
//   bb0(%0 : @owned $NSObject, %1 : @unowned $Handler):
//     %2 = copy_value %1           // only self copied — no retain on %0
//     apply %4(%0, %3)             // forwards +0 where +1 expected — BUG
//
// After the fix:
//   bb0(%0 : @unowned $NSObject, %1 : @unowned $Handler):
//     %2 = copy_value %0           // retain sending param +0 → +1
//     %3 = copy_value %1           // retain self
//     apply %5(%2, %4)             // forwards +1 copy — correct
//
// CHECK-LABEL: sil private [thunk] [ossa] @$s12objc_sending7HandlerC6handleyySo8NSObjectCnFTo : $@convention(objc_method) (@sil_sending NSObject, Handler) -> () {
// CHECK: bb0([[SENDING_ARG:%.*]] : @unowned $NSObject, [[SELF_ARG:%.*]] : @unowned $Handler):
// CHECK-NEXT:   [[SENDING_COPY:%.*]] = copy_value [[SENDING_ARG]] : $NSObject
// CHECK-NEXT:   [[SELF_COPY:%.*]] = copy_value [[SELF_ARG]] : $Handler
// CHECK-NEXT:   [[SELF_BORROW:%.*]] = begin_borrow [[SELF_COPY]] : $Handler
// CHECK:        [[NATIVE_FN:%.*]] = function_ref @$s12objc_sending7HandlerC6handleyySo8NSObjectCnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Handler) -> ()
// CHECK-NEXT:   apply [[NATIVE_FN]]([[SENDING_COPY]], [[SELF_BORROW]]) : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Handler) -> ()
// CHECK-NEXT:   end_borrow [[SELF_BORROW]] : $Handler
// CHECK-NEXT:   destroy_value [[SELF_COPY]] : $Handler
// CHECK:      } // end sil function

// ============================================================================
// Case 2: Pure Swift @objc method with `sending` — no ObjC header involved.
// This exercises ObjCSelectorFamilyConventions (no clang::ObjCMethodDecl).
// The same bug applies: ObjC callers deliver at +0, thunk must not assume +1.
// ============================================================================

import Foundation

class Processor: NSObject {
  @objc func process(_ value: sending NSObject) {}
}

// The native Swift entry point expects @sil_sending @owned.
// CHECK-LABEL: sil hidden [ossa] @$s12objc_sending9ProcessorC7processyySo8NSObjectCnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Processor) -> ()

// The @objc thunk — same fix applies: sending param must be @unowned, not @owned.
// CHECK-LABEL: sil private [thunk] [ossa] @$s12objc_sending9ProcessorC7processyySo8NSObjectCnFTo : $@convention(objc_method) (@sil_sending NSObject, Processor) -> () {
// CHECK: bb0([[SENDING_ARG:%.*]] : @unowned $NSObject, [[SELF_ARG:%.*]] : @unowned $Processor):
// CHECK-NEXT:   [[SENDING_COPY:%.*]] = copy_value [[SENDING_ARG]] : $NSObject
// CHECK-NEXT:   [[SELF_COPY:%.*]] = copy_value [[SELF_ARG]] : $Processor
// CHECK-NEXT:   [[SELF_BORROW:%.*]] = begin_borrow [[SELF_COPY]] : $Processor
// CHECK:        [[NATIVE_FN:%.*]] = function_ref @$s12objc_sending9ProcessorC7processyySo8NSObjectCnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Processor) -> ()
// CHECK-NEXT:   apply [[NATIVE_FN]]([[SENDING_COPY]], [[SELF_BORROW]]) : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Processor) -> ()
// CHECK-NEXT:   end_borrow [[SELF_BORROW]] : $Processor
// CHECK-NEXT:   destroy_value [[SELF_COPY]] : $Processor
// CHECK:      } // end sil function
