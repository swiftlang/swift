// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/CUSTOM_DIR)

// RUN: %target-swift-frontend  -I %t/lib/swift -typecheck %s %S/Inputs/objc_send_collector_2.swift -module-name main -swift-version 5 -F %S/Inputs/mock-sdk -emit-loaded-module-trace-path %t/.MODULE_TRACE
// RUN: cat %t/.SWIFT_FINE_DEPENDENCY_TRACE/* | %FileCheck %s

// RUN: SWIFT_COMPILER_OBJC_MESSAGE_TRACE_DIRECTORY=%t/CUSTOM_DIR %target-swift-frontend  -I %t/lib/swift -typecheck %s %S/Inputs/objc_send_collector_2.swift -module-name main -swift-version 5 -F %S/Inputs/mock-sdk -emit-loaded-module-trace-path %t/.MODULE_TRACE
// RUN: cat %t/CUSTOM_DIR/* | %FileCheck %s

// REQUIRES: objc_interop

import Foo

public func testProperties(_ x: FooClassBase, _ y: FooProtocolBase) {
  _ = x.fooBaseInstanceFunc0()
  x.fooBaseInstanceFunc1(1.2)
  _ = FooClassBase.fooBaseClassFunc0()
  y.fooProtoFunc()
}

// CHECK-DAG: "instance_method": "-[FooClassBase fooBaseInstanceFunc0]"
// CHECK-DAG: "instance_method": "-[FooClassBase fooBaseInstanceFunc1:]"
// CHECK-DAG: "class_method": "+[FooClassBase fooBaseClassFunc0]"
// CHECK-DAG: "interface_type": "FooClassBase"
// CHECK-DAG: "protocol_type": "FooProtocolBase"
// CHECK-DAG: "declared_at": "SOURCE_DIR/test/IDE/Inputs/mock-sdk/Foo.framework/Headers/Foo.h
// CHECK-DAG: "referenced_at_file_id": 1
// CHECK-DAG: "referenced_at_file_id": 2
// CHECK-DAG: "file_id": 1,
// CHECK-DAG: "file_path": "SOURCE_DIR/test/IDE/objc_send_collector_1.swift"
// CHECK-DAG: "file_path": "SOURCE_DIR/test/IDE/Inputs/objc_send_collector_2.swift"
// CHECK-DAG: "swift-compiler-version":
