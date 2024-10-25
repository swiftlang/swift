// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend  -I %t/lib/swift -typecheck %s -module-name main -swift-version 5 -F %S/Inputs/mock-sdk -emit-loaded-module-trace-path %t/.MODULE_TRACE
// RUN: %{python} %S/Inputs/print_first_file_under_directory.py %t/SWIFT_OBJC_MESSAGE_TRACE | %FileCheck %s

// REQUIRES: objc_interop

// EXPECTED OUTPUT STARTS BELOW THIS LINE.


import Foo

public func testProperties(_ x: FooClassBase) {
  _ = x.fooBaseInstanceFunc0()
  x.fooBaseInstanceFunc1(1.2)
}

// CHECK: fooBaseInstanceFunc0
// CHECK: fooBaseInstanceFunc1
// CHECK: SOURCE_DIR/test/IDE/Inputs/mock-sdk/Foo.framework/Headers/Foo.h
