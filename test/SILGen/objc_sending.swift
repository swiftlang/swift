// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -import-objc-header %S/Inputs/objc_sending.h %s | %FileCheck %s

// REQUIRES: objc_interop

// The @objc thunk incorrectly declares the `sending` parameter as @owned
// (because `sending` promotes to ValueOwnership::Owned → Direct_Owned).
// But ObjC delivers all arguments at +0 via objc_msgSend, so the thunk
// forwards a +0 value where the native Swift body expects +1 → double-free.
//
// This test captures the BUGGY behavior: the thunk receives %0 as @owned
// but only emits copy_value for self (%1), not for the sending argument (%0).
// The sending argument is forwarded directly without a retain — unbalanced.

class Handler: NSObject, ObjCHandlerWithSending {
  func handle(_ value: sending NSObject) {}
}

// The native Swift entry point correctly expects @sil_sending @owned.
// CHECK-LABEL: sil hidden [ossa] @$s12objc_sending7HandlerC6handleyySo8NSObjectCnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Handler) -> ()

// The @objc thunk — this is where the bug manifests.
//
// BUG: The thunk declares @sil_sending @owned, but ObjC delivers at +0.
//      Only self gets copy_value; the sending param does NOT.
//      The sending param is forwarded to the native body without a retain,
//      causing a double-free when the native body consumes it.
//
// CHECK-LABEL: sil private [thunk] [ossa] @$s12objc_sending7HandlerC6handleyySo8NSObjectCnFTo : $@convention(objc_method) (@sil_sending @owned NSObject, Handler) -> () {
//
//   %0 is @owned — but ObjC actually delivered it at +0. This is the bug.
// CHECK: bb0(%0 : @owned $NSObject, %1 : @unowned $Handler):
//
//   Only self (%1) gets a copy_value. The sending param (%0) does NOT.
// CHECK-NEXT:   [[SELF_COPY:%.*]] = copy_value %1 : $Handler
// CHECK-NEXT:   [[SELF_BORROW:%.*]] = begin_borrow [[SELF_COPY]] : $Handler
//
//   %0 is forwarded directly to the native body without any retain.
//   The native body will consume it (release), over-releasing the +0 value.
// CHECK:        [[NATIVE:%.*]] = function_ref @$s12objc_sending7HandlerC6handleyySo8NSObjectCnF
// CHECK-NEXT:   apply [[NATIVE]](%0, [[SELF_BORROW]])
// CHECK-NEXT:   end_borrow [[SELF_BORROW]] : $Handler
// CHECK-NEXT:   destroy_value [[SELF_COPY]] : $Handler
// CHECK:      } // end sil function
