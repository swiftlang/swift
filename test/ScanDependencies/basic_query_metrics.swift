// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/test.swift -o %t/deps.json -I %t/inputs -Rdependency-scan &> %t/remarks.txt
// RUN: cat %t/remarks.txt | %FileCheck %s

// Ensure that despite being a common dependency to multiple Swift modules, only 1 query is performed to find 'C'
// CHECK: remark: Number of Swift module queries: '3'
// CHECK: remark: Number of named Clang module queries: '1'
// CHECK: remark: Number of recorded Clang module dependencies queried by-name from a Swift client: '1'
// CHECK: remark: Number of recorded Swift module dependencies: '2'
// CHECK: remark: Number of recorded Clang module dependencies: '1'

//--- test.swift
import A
import B
public func test() {}

//--- inputs/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
import C
public func a() { }

//--- inputs/B.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name B -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
import C
public func b() { }

//--- inputs/C.h
void c(void);

//--- inputs/module.modulemap
module C {
  header "C.h"
  export *
}

