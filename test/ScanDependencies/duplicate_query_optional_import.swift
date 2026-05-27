// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/inputs/Foo.swiftmodule)
// RUN: %empty-directory(%t/inputs/Bar.swiftmodule)
// RUN: split-file %s %t

// Step 1: Build binary module Foo with 'internal import C' and a .swiftinterface
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -emit-module-path %t/inputs/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo -emit-module-interface-path %t/inputs/Foo.swiftmodule/%target-swiftinterface-name -I %t/inputs -enable-testing -enable-library-evolution -swift-version 5 -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -parse-stdlib

// Step 2: Build binary module Bar with 'internal import C' and a .swiftinterface
// RUN: %target-swift-frontend -emit-module %t/Bar.swift -emit-module-path  %t/inputs/Bar.swiftmodule/%target-swiftmodule-name -module-name Bar -emit-module-interface-path %t/inputs/Bar.swiftmodule/%target-swiftinterface-name -I %t/inputs -enable-testing -enable-library-evolution -swift-version 5 -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -parse-stdlib

// Step 3: Scan dependencies with @testable imports
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/test.swift -o %t/deps.json -I %t/inputs -Rdependency-scan &> %t/remarks.txt
// RUN: cat %t/remarks.txt | %FileCheck %s

// Ensure that the optional import 'C' (internal import in both Foo and Bar)
// is only queried once despite appearing as an optional dependency of both
// Foo and Bar.
//
// CHECK: remark: Number of Swift module queries: '3'

//--- test.swift
@testable import Foo
@testable import Bar
public func test() {}

//--- Foo.swift
internal import C
public func foo() {}

//--- Bar.swift
internal import C
public func bar() {}

//--- inputs/C.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name C -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func c() { }
