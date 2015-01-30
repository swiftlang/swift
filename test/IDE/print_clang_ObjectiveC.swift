// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=ObjectiveC.NSObject -function-definitions=false > %t/ObjectiveC.NSObject.printed.txt
// RUN: FileCheck -input-file %t/ObjectiveC.NSObject.printed.txt %s
// RUN: FileCheck -input-file %t/ObjectiveC.NSObject.printed.txt -check-prefix=NEGATIVE -check-prefix=NEGATIVE-WITHOUT-FORWARD-DECLS %s

// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print=ObjectiveC.NSObject -function-definitions=false -enable-objc-forward-declarations > %t/ObjectiveC.NSObject.forward-decls.txt
// RUN: FileCheck -input-file %t/ObjectiveC.NSObject.forward-decls.txt -check-prefix=CHECK -check-prefix=CHECK-WITH-FORWARD-DECLS %s
// RUN: FileCheck -input-file %t/ObjectiveC.NSObject.forward-decls.txt -check-prefix=NEGATIVE %s

// REQUIRES: objc_interop

// NEGATIVE-WITHOUT-FORWARD-DECLS-NOT: var description
// NEGATIVE-NOT: NSCoder

// CHECK-LABEL: protocol NSObjectProtocol {
// CHECK-DAG: var superclass: AnyClass? { get }
// CHECK-DAG: func zone() -> NSZone
// CHECK-WITH-FORWARD-DECLS-DAG: var description: String { get }
// CHECK: {{^[}]$}}

// CHECK-LABEL: class NSObject : NSObjectProtocol {
// CHECK-DAG: func copy() -> AnyObject
// CHECK-DAG: class func hash() -> Int
// CHECK-WITH-FORWARD-DECLS-DAG: class func description() -> String
// CHECK-WITH-FORWARD-DECLS-DAG: func forwardInvocation(anInvocation: NSInvocation!)
// CHECK: {{^[}]$}}
