// RUN: %empty-directory(%t)
// RUN: %target-clang -fobjc-arc %S/Inputs/objc_async.m -c -o %t/objc_async_objc.o

// Default mode:
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -parse-as-library -module-name main -import-objc-header %S/Inputs/objc_async.h %s %t/objc_async_objc.o -o %t/objc_async_default
// RUN: %target-codesign %t/objc_async_default
// RUN: %target-run %t/objc_async_default | %FileCheck %s

// Select 'task' mode (same as default):
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -parse-as-library -module-name main -import-objc-header %S/Inputs/objc_async.h %s %t/objc_async_objc.o -o %t/objc_async_task
// RUN: %target-codesign %t/objc_async_task
// RUN: %target-run %t/objc_async_task | %FileCheck --check-prefix=CHECK-TASK %s

// Select 'immediate' mode via upcoming feature:
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -parse-as-library -module-name main -Xfrontend -enable-upcoming-feature -Xfrontend ObjCAsyncBridgeImmediateTask -import-objc-header %S/Inputs/objc_async.h %s %t/objc_async_objc.o -o %t/objc_async_task_immediate
// RUN: %target-codesign %t/objc_async_task_immediate
// RUN: %target-run %t/objc_async_task_immediate | %FileCheck --check-prefix=CHECK-IMMEDIATE %s

// REQUIRES: swift_feature_ObjCAsyncBridgeImmediateTask

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Foundation // Thread

class SwiftSubclass: MyNSInterfaceWithCallbackFunc {
  override func compute(_ x: Int) async -> Int {
    print("ran on main thread: \(Thread.isMainThread)")
    return x + 1
  }
}

// Default mode (same as 'task'):
// CHECK: ran on main thread: false
// CHECK-NEXT: result: 129

// Force using Task:
// Task enqueues to the global concurrent executor (background thread).
// CHECK-TASK: ran on main thread: false
// CHECK-TASK-NEXT: result: 129

// Force using Task.immediate:
// Task.immediate runs synchronously on the calling (main) thread.
// CHECK-IMMEDIATE: ran on main thread: true
// CHECK-IMMEDIATE-NEXT: result: 129

@main struct Main {
  static func main() {
    let result = callComputeAndWaitSemaphore(SwiftSubclass(), 128)
    print("result: \(result)")
  }
}
