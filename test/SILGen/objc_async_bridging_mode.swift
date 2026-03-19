// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -emit-silgen -I %S/Inputs/custom-modules -target %target-swift-5.1-abi-triple %s | %FileCheck --check-prefix=CHECK-TASK %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -emit-silgen -I %S/Inputs/custom-modules -target %target-swift-5.1-abi-triple -objc-call-swift-async-bridging=task %s | %FileCheck --check-prefix=CHECK-TASK %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -emit-silgen -I %S/Inputs/custom-modules -target %target-swift-5.1-abi-triple -objc-call-swift-async-bridging=task-immediate %s | %FileCheck --check-prefix=CHECK-IMMEDIATE %s

// REQUIRES: concurrency
// REQUIRES: objc_interop

import Foundation

class MyServer: NSObject {
    // --- Default / explicit "task" mode: uses legacy _runTaskForBridgedAsyncMethod
    // CHECK-TASK-LABEL: sil private [thunk] [ossa] @$s24objc_async_bridging_mode8MyServerC0B4FuncSiyYaFTo :
    // CHECK-TASK:         function_ref @$ss29_runTaskForBridgedAsyncMethodyyyyYaYbcnF
    // CHECK-TASK:         apply

    // --- "task-immediate" mode: uses _runTaskImmediateForBridgedAsyncMethod
    // CHECK-IMMEDIATE-LABEL: sil private [thunk] [ossa] @$s24objc_async_bridging_mode8MyServerC0B4FuncSiyYaFTo :
    // CHECK-IMMEDIATE:         function_ref @$ss38_runTaskImmediateForBridgedAsyncMethodyyyyYaYbcnF
    // CHECK-IMMEDIATE:         apply

    @objc func asyncFunc() async -> Int { 42 }

    // CHECK-TASK-LABEL: sil private [thunk] [ossa] @$s24objc_async_bridging_mode8MyServerC0B12ThrowingFuncSiyYaKFTo :
    // CHECK-TASK:         function_ref @$ss29_runTaskForBridgedAsyncMethodyyyyYaYbcnF
    // CHECK-TASK:         apply

    // CHECK-IMMEDIATE-LABEL: sil private [thunk] [ossa] @$s24objc_async_bridging_mode8MyServerC0B12ThrowingFuncSiyYaKFTo :
    // CHECK-IMMEDIATE:         function_ref @$ss38_runTaskImmediateForBridgedAsyncMethodyyyyYaYbcnF
    // CHECK-IMMEDIATE:         apply

    @objc func asyncThrowingFunc() async throws -> Int { 42 }
}
