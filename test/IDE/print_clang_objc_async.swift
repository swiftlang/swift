// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -source-filename %s -module-to-print=ObjCConcurrency -function-definitions=false -enable-experimental-concurrency > %t/ObjCConcurrency.printed.txt
// RUN: %FileCheck -input-file %t/ObjCConcurrency.printed.txt %s

// REQUIRES: objc_interop
// REQUIRES: concurrency

// CHECK-LABEL: class SlowServer : NSObject, ServiceProvider {
// CHECK-DAG:     func doSomethingSlow(_ operation: String, completionHandler handler: @escaping (Int) -> Void)
// CHECK-DAG:     func doSomethingSlow(_ operation: String) async -> Int
// CHECK-DAG:     func doSomethingDangerous(_ operation: String, completionHandler handler: ((String?, Error?) -> Void)? = nil)
// CHECK-DAG:     func doSomethingDangerous(_ operation: String) async throws -> String
// CHECK-DAG:     func checkAvailability(completionHandler: @escaping (Bool) -> Void)
// CHECK-DAG:     func checkAvailability() async -> Bool
// CHECK-DAG:     func anotherExample() async -> String
// CHECK-DAG:     func finalExample() async -> String
// CHECK-DAG:     func replyingOperation(_ operation: String) async -> String
// CHECK-DAG:     func findAnswer(completionHandler handler: @escaping (String?, Error?) -> Void)
// CHECK-DAG:     func findAnswer() async throws -> String
// CHECK-DAG:     func findAnswerFailingly(completionHandler handler: @escaping (String?, Error?) -> Void) throws
// CHECK-DAG:     func findAnswerFailingly() async throws -> String
// CHECK-DAG:     func findQAndA() async throws -> (String?, String)
// CHECK-DAG:     func findQuestionableAnswers() async throws -> (String, String?)
// CHECK-DAG:     func doSomethingFun(_ operation: String) async
// CHECK-DAG:     func dance(_ step: String) async -> String
// CHECK-DAG:     func __leap(_ height: Int) async -> String
// CHECK: {{^[}]$}}

// CHECK-LABEL: protocol RefrigeratorDelegate
// CHECK-NEXT: @asyncHandler func someoneDidOpenRefrigerator(_ fridge: Any)
// CHECK-NEXT: @asyncHandler func refrigerator(_ fridge: Any, didGetFilledWithItems items: [Any])
// CHECK-NEXT: {{^}}  func refrigerator(_ fridge: Any, didGetFilledWithIntegers items: UnsafeMutablePointer<Int>, count: Int)
// CHECK-NEXT: {{^}}  func refrigerator(_ fridge: Any, willAddItem item: Any)
// CHECK-NEXT: {{^}}  func refrigerator(_ fridge: Any, didRemoveItem item: Any) -> Bool
// CHECK-NEXT: {{^[}]$}}
