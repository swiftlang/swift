// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/clang-module-cache)
// RUN: %empty-directory(%t/Foo.swiftmodule)
// RUN: echo "@_implementationOnly import A; public func foo() {}" > %t/Foo.swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

@testable import Foo

// Step 1: build a binary swift module for `Foo`, make it testable
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -emit-module-path %t/Foo.swiftmodule/%target-swiftmodule-name -module-name Foo -I %S/Inputs/CHeaders -I %S/Inputs/Swift -enable-testing

// Step 2: scan dependencies
// RUN: %target-swift-frontend -scan-dependencies %s -o %t/deps.json -I %t -sdk %t -prebuilt-module-cache-path %t/clang-module-cache -I %S/Inputs/CHeaders -I %S/Inputs/Swift
// RUN: %validate-json %t/deps.json | %FileCheck %s

// The dependency of `Foo` on `A` will not be visible if the scanner simply scans the textual interface
// of `Foo`. So we verify that for a `@testable` import, the scanner also opens up the adjacent binary module and
// attemtps to resolve optional dependencies contained within.
//
// CHECK: "swift": "A"
