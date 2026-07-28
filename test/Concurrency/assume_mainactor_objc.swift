// RUN: %target-swift-frontend -swift-version 5 -emit-silgen -default-isolation MainActor %s | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 5 -strict-concurrency=complete -emit-silgen -default-isolation MainActor %s | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 6 -emit-silgen -default-isolation MainActor %s | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 6 -emit-sil -default-isolation MainActor %s -verify

// assume_mainactor.swift with objc_interop
// Please keep this file to FileCheck of isolation under -default-isolation MainActor, and only test objc interop specific things.
// This test is able to check Swift 5 because it doesn't interact with mode dependent inits...

// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation

class SwiftClass {
  // CHECK: // SwiftClass.__isolated_deallocating_deinit
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
}

class ObjCSub: NSObject {
  // CHECK: // ObjCSub.init()
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden [ossa] @$s21assume_mainactor_objc7ObjCSubCACycfc : $@convention(method) (@owned ObjCSub) -> @owned ObjCSub {

  // Deinit has no isolation due to NSObject deinit being nonisolated.
  // CHECK: // ObjCSub.__deallocating_deinit
  // CHECK-NEXT: // Isolation: nonisolated
  // CHECK-NEXT: sil hidden [ossa] @$s21assume_mainactor_objc7ObjCSubCfD : $@convention(method) (@owned ObjCSub) -> () {

  // CHECK-NOT: __isolated_deallocating_deinit
  // CHECK-NOT: swift_task_deinitOnExecutor
  // CHECK: } // end sil function '$s21assume_mainactor_objc7ObjCSubCfD'
}
