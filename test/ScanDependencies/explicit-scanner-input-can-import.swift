// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/Inputs/Foo.swiftmodule)
// RUN: split-file %s %t

// Step 1: build swift interface and swift module side by side
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -emit-module-path %t/Inputs/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo -user-module-version 22

// Step 2: scan dependency should give us the binary module we specify with 'swift-module-file'
// RUN: %target-swift-frontend -scan-dependencies %t/test.swift -o %t/deps.json -scanner-module-validation -swift-module-file=Foo=%t/Inputs/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %validate-json %t/deps.json | %FileCheck %s -check-prefix=CHECK-INPUT

// Step 3: ensure that versioned canImport still applies with direct -swift-module-file
// RUN: %target-swift-frontend -scan-dependencies %t/test_too_new.swift -o %t/deps.json -scanner-module-validation -swift-module-file=Foo=%t/Inputs/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %validate-json %t/deps.json | %FileCheck %s -check-prefix=CHECK-MISSING

// CHECK-INPUT: "swiftPrebuiltExternal": "Foo"
// CHECK-MISSING-NOT: "swiftPrebuiltExternal": "Foo"

//--- Foo.swift
public func foo() {}

//--- test.swift
#if canImport(Foo)
import Foo
#endif

//--- test_too_new.swift
#if canImport(Foo, _version: 23)
import Foo
#endif
