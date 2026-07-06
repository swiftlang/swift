// RUN: %empty-directory(%t)

// RUN: split-file --leading-lines %s %t

// RUN: env SWIFT_INDEX_STORE_PATH=%t/idx-with-multiple-options %target-swift-frontend -typecheck %t/test.swift -I%t
// RUN: c-index-test core -print-record %t/idx-with-multiple-options | %FileCheck %s

//--- module.modulemap

module MyModule {
  header "MyHeader.h"
  export *
}

//--- MyHeader.h

// CHECK-DAG: [[@LINE+1]]:6 | function/C
void myTestFunc() {}

//--- test.swift

import MyModule

// CHECK-DAG: [[@LINE+1]]:6 | function/Swift
func test() {
  myTestFunc()
}
