// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend  -I %t/lib/swift -typecheck %s -module-name main -swift-version 5 -F %S/Inputs/mock-sdk -emit-loaded-module-trace-path %t/.MODULE_TRACE
// RUN: cat %t/.SWIFT_OBJC_MESSAGE_TRACE/* | %FileCheck %s

// REQUIRES: objc_interop

import Foo

public func testProperties(_ x: FooClassBase) {
  _ = x.fooBaseInstanceFunc0()
  x.fooBaseInstanceFunc1(1.2)
}

// CHECK-DAG: fooBaseInstanceFunc0
// CHECK-DAG: fooBaseInstanceFunc1
// CHECK-DAG: SOURCE_DIR/test/IDE/Inputs/mock-sdk/Foo.framework/Headers/Foo.h
