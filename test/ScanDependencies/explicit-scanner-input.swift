// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/Inputs/Foo.swiftmodule)
// RUN: split-file %s %t

// Step 1: build swift interface and swift module side by side
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -emit-module-path %t/Inputs/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo

// Step 2: scan dependency should give us the binary module we specify with 'swift-module-file'
// RUN: %target-swift-frontend -scan-dependencies %t/test.swift -o %t/deps.json -scanner-module-validation -swift-module-file=Foo=%t/Inputs/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %validate-json %t/deps.json | %FileCheck %s -check-prefix=CHECK-INPUT
// CHECK-INPUT: "swiftPrebuiltExternal": "Foo"

//--- Foo.swift
public func foo() {}

//--- test.swift
#if canImport(Foo)
import Foo
#endif
