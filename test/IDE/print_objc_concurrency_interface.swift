// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -print-interface -source-filename %s -module-to-print=ObjCConcurrency -function-definitions=false  | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: concurrency
import _Concurrency

// CHECK-LABEL: class SlowServer : NSObject, ServiceProvider {

// rdar://76685011: Make sure we don't print @completionHandlerAsync in generated interfaces.
// CHECK-NOT: @completionHandlerAsync
// CHECK: func doSomethingSlow(_ operation: String, completionHandler handler: @escaping (Int) -> Void)
// CHECK: func doSomethingSlow(_ operation: String) async -> Int
