// RUN: %empty-directory(%t)

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -print-module -print-interface -source-filename %s -module-to-print=ObjCConcurrency -function-definitions=false -enable-experimental-feature SendableCompletionHandlers > %t/ObjCConcurrency.printed.txt
// RUN: %FileCheck -input-file %t/ObjCConcurrency.printed.txt %s
// RUN: %FileCheck -check-prefix NEGATIVE -input-file %t/ObjCConcurrency.printed.txt %s


// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: swift_feature_SendableCompletionHandlers
import _Concurrency

// CHECK-LABEL: class SlowServer : NSObject, ServiceProvider {

// rdar://76685011: Make sure we don't print implicit @available in generated interfaces.
// CHECK-NOT: @available
// CHECK: func doSomethingSlow(_ operation: String, completionHandler handler: @escaping @Sendable (Int) -> Void)
// CHECK: func doSomethingSlow(_ operation: String) async -> Int

// NEGATIVE-NOT: @Sendable{{.+}}class
// NEGATIVE-NOT: @_nonSendable{{.+}}class
// NEGATIVE-NOT: @Sendable{{.+}}func
// NEGATIVE-NOT: @Sendable{{.+}}var

// CHECK-LABEL: class SendableClass :
// CHECK-SAME: @unchecked Sendable

// CHECK-LABEL: class NonSendableClass
// CHECK-NOT: @unchecked Sendable
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension NonSendableClass : @unchecked Sendable {

// CHECK-LABEL: class AuditedSendable :
// CHECK-SAME: @unchecked Sendable

// CHECK-LABEL: class AuditedNonSendable
// CHECK-NOT: @unchecked Sendable
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension AuditedNonSendable : @unchecked Sendable {

// CHECK-LABEL: class AuditedBoth
// CHECK-NOT: @unchecked Sendable
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension AuditedBoth : @unchecked Sendable {

// CHECK-LABEL: public protocol SendableProtocol
// CHECK-SAME: : Sendable

// CHECK-LABEL: enum SendableEnum :
// CHECK-SAME: @unchecked Sendable

// CHECK-LABEL: enum NonSendableEnum :
// CHECK-NOT: @unchecked Sendable
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension NonSendableEnum : @unchecked Sendable {

// CHECK-LABEL: struct SendableOptions :
// CHECK-SAME: @unchecked Sendable

// CHECK-LABEL: struct NonSendableOptions :
// CHECK-NOT: @unchecked Sendable
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension NonSendableOptions : @unchecked Sendable {

// CHECK-LABEL: public struct SendableError :
// CHECK-NOT: @unchecked Sendable
// CHECK: public enum Code :
// CHECK-SAME: @unchecked Sendable

// CHECK-LABEL: public struct NonSendableError :
// CHECK-NOT: @unchecked Sendable
// CHECK: public enum Code :
// CHECK-SAME: @unchecked Sendable

// CHECK-LABEL: struct SendableStringEnum :
// CHECK-SAME: @unchecked Sendable

// CHECK-LABEL: struct NonSendableStringEnum :
// CHECK-NOT: @unchecked Sendable
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension NonSendableStringEnum : @unchecked Sendable {

// CHECK-LABEL: struct SendableStringStruct :
// CHECK-SAME: @unchecked Sendable

// CHECK-LABEL: struct NonSendableStringStruct :
// CHECK-NOT: @unchecked Sendable
// CHECK: @available(*, unavailable)
// CHECK-NEXT: extension NonSendableStringStruct : @unchecked Sendable {

