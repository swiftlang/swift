// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -source-filename %s -module-to-print=ObjCConcurrency -function-definitions=false -enable-experimental-concurrency > %t/ObjCConcurrency.printed.txt
// RUN: %FileCheck -input-file %t/ObjCConcurrency.printed.txt %s

// REQUIRES: objc_interop

// CHECK-LABEL: class SlowServer : NSObject {
// CHECK-DAG:     func doSomethingSlow(_ operation: String, completionHandler handler: @escaping (Int) -> Void)
// CHECK-DAG:     func doSomethingSlow(_ operation: String) async -> Int
// CHECK-DAG:     func doSomethingDangerous(_ operation: String, completionHandler handler: ((String?, Error?) -> Void)? = nil)
// CHECK-DAG:     func doSomethingDangerous(_ operation: String) async throws -> String?
// CHECK-DAG:     func checkAvailability(completionHandler: @escaping (Bool) -> Void)
// CHECK-DAG:     func checkAvailability() async -> Bool
// CHECK-DAG:     func findAnswer(completionHandler handler: @escaping (String?, Error?) -> Void)
// CHECK-DAG:     func findAnswer() async throws -> String?
// CHECK-DAG:     func findAnswerFailingly(completionHandler handler: @escaping (String?, Error?) -> Void) throws
// CHECK-DAG:     func findAnswerFailingly() async throws -> String?
// CHECK-DAG:     func doSomethingFun(_ operation: String) async
// CHECK: {{^[}]$}}
