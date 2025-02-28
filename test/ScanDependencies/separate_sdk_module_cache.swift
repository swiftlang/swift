// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/sdk-module-cache)
// RUN: %empty-directory(%t/mock.sdk/System/Library/Frameworks/FooBar.framework/Modules/FooBar.swiftmodule)
// RUN: split-file %s %t
// RUN: cp %t/FooBar.swiftinterface %t/mock.sdk/System/Library/Frameworks/FooBar.framework/Modules/FooBar.swiftmodule/%target-swiftinterface-name

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/test.swift -o %t/deps.json -I %S/Inputs/Swift -sdk %t/mock.sdk -sdk-module-cache-path %t/sdk-module-cache
// RUN: %validate-json %t/deps.json | %FileCheck %s

//--- FooBar.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name FooBar -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -user-module-version 1.0
public func foo() {}

//--- test.swift
import E
import FooBar

// CHECK-DAG: "modulePath": "{{.*}}{{/|\\}}module-cache{{/|\\}}E-{{.*}}.swiftmodule"
// CHECK-DAG: "modulePath": "{{.*}}{{/|\\}}sdk-module-cache{{/|\\}}FooBar-{{.*}}.swiftmodule"
