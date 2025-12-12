// REQUIRES: asserts
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/test.swift -o %t/deps.json -I %t/inputs -print-stats &> %t/stats.txt
// RUN: cat %t/stats.txt | %FileCheck %s

// Ensure that despite being a common dependency to multiple Swift modules, only 1 query is performed to find 'C'
// CHECK: ... Statistics Collected ...
// CHECK:    1 DependencyScanner - # of times the dependency scanner performed a named lookup of a Clang module

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

//--- inputs/c.h
void c(void);

//--- inputs/module.modulemap
module C {
  header "C.h"
  export *
}

