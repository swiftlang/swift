// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -print-interface -source-filename %s -module-to-print=ObjCConcurrency -function-definitions=false > %t/ObjCConcurrency.printed.txt
// RUN: %FileCheck -input-file %t/ObjCConcurrency.printed.txt %s
// RUN: %FileCheck -check-prefix NEGATIVE -input-file %t/ObjCConcurrency.printed.txt %s


// REQUIRES: objc_interop
// REQUIRES: concurrency
import _Concurrency

// CHECK-LABEL: class SlowServer : NSObject, ServiceProvider {

// rdar://76685011: Make sure we don't print implicit @available in generated interfaces.
// CHECK-NOT: @available
// CHECK: func doSomethingSlow(_ operation: String, completionHandler handler: @escaping (Int) -> Void)
// CHECK: func doSomethingSlow(_ operation: String) async -> Int

// NEGATIVE-NOT: @Sendable{{.+}}class
// NEGATIVE-NOT: @_nonSendable{{.+}}class

// CHECK-LABEL: class SendableClass :
// CHECK-SAME: @unchecked Sendable

// CHECK-LABEL: class NonSendableClass
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension NonSendableClass : @unchecked Sendable {

// CHECK-LABEL: class AuditedSendable :
// CHECK-SAME: @unchecked Sendable

// CHECK-LABEL: class AuditedNonSendable
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension AuditedNonSendable : @unchecked Sendable {

// CHECK-LABEL: class AuditedBoth
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension AuditedBoth : @unchecked Sendable {

