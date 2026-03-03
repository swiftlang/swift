// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/test.swift -o %t/deps.json -I %t/inputs -Rdependency-scan &> %t/remarks.txt
// RUN: cat %t/remarks.txt | %FileCheck %s

// Ensure that although the Swift overlay dependency 'A' shares a dependency on 'B' and 'C'
// it does not incur additional namedqueries for them.
//
// CHECK: remark: Number of Swift module queries: '3'
// CHECK: remark: Number of named Clang module queries: '2'
// CHECK: remark: Number of recorded Clang module dependencies queried by-name from a Swift client: '2'
// CHECK: remark: Number of recorded Swift module dependencies: '1'
// CHECK: remark: Number of recorded Clang module dependencies: '3'

//--- test.swift
import B
import C
public func test() {}

//--- inputs/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
import B
import C
public func a() { }

//--- inputs/A.h
void b(void);

//--- inputs/B.h
#include "A.h"
void b(void);

//--- inputs/C.h
void c(void);

//--- inputs/module.modulemap
module A {
  header "A.h"
  export *
}
module B {
  header "B.h"
  export *
}
module C {
  header "C.h"
  export *
}
