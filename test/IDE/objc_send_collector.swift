// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend  -I %t/lib/swift -typecheck %s -module-name main -swift-version 5 -F %S/Inputs/mock-sdk -emit-loaded-module-trace-path %t/.MODULE_TRACE
// RUN: cat %t/.SWIFT_OBJC_MESSAGE_TRACE/* | %FileCheck %s

// REQUIRES: objc_interop

import Foo

public func testProperties(_ x: FooClassBase) {
  _ = x.fooBaseInstanceFunc0()
  x.fooBaseInstanceFunc1(1.2)
  _ = FooClassBase.fooBaseClassFunc0()
}

// CHECK-DAG: "instance_method": "fooBaseInstanceFunc0"
// CHECK-DAG: "instance_method": "fooBaseInstanceFunc1:"
// CHECK-DAG: "class_method": "fooBaseClassFunc0"
// CHECK-DAG: "interface_type": "FooClassBase"
// CHECK-DAG: "declared_at": "SOURCE_DIR/test/IDE/Inputs/mock-sdk/Foo.framework/Headers/Foo.h
// CHECK-DAG: "referenced_at": "SOURCE_DIR/test/IDE/objc_send_collector.swift"
