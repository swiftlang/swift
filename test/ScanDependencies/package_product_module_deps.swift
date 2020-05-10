// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -scan-dependencies %s -o %t/deps.json -I %S/Inputs/Swift -disable-implicit-swift-modules

// Check the contents of the JSON output
// RUN: %FileCheck %s < %t/deps.json

// Check that the JSON parses correctly into the canonical Swift data
// structures.

// RUN: mkdir -p %t/PrintGraph
// RUN: cp %S/Inputs/PrintGraph.swift %t/main.swift
// RUN: %target-build-swift %S/Inputs/ModuleDependencyGraph.swift %t/main.swift -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/deps.json

// REQUIRES: executable_test
// REQUIRES: objc_interop

@_package(url: "https://www.xyz.com/my/awesome/package.git", .exact("1.0.0"))
import MyAwesomeLibrary

@_package(url: "https://www.xyz.com/my/other/package.git", .exact("1.2.3"))
import MyOtherAwesomeLibrary

// Make sure we correctly associate unattributed imports with the attributed import.
import MyAwesomeLibrary

// CHECK: "mainModuleName": "deps"

/// --------Main module
// CHECK-LABEL: "modulePath": "deps.swiftmodule",
// CHECK-NEXT: sourceFiles
// CHECK-NEXT: module_deps.swift

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "swiftPackageProduct": "MyAwesomeLibrary"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swiftPackageProduct": "MyOtherAwesomeLibrary"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "Swift"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "SwiftOnoneSupport"
// CHECK-NEXT: }
// CHECK-NEXT: ],

/// --------Swift package product MyAwesomeLibrary
// CHECK: "swiftPackageProduct": "MyAwesomeLibrary"
// CHECK-LABEL: modulePath": ""
// CHECK-NEXT: "details": {
// CHECK-NEXT: "swiftPackageProduct": {
// CHECK-NEXT:     "productDescription": "(url: \"https://www.xyz.com/my/awesome/package.git\", .exact(\"1.0.0\"))"
// CHECK-NEXT:   }
// CHECK-NEXT: }

/// --------Swift package product MyOtherAwesomeLibrary
// CHECK: "swiftPackageProduct": "MyOtherAwesomeLibrary"
// CHECK-LABEL: modulePath": ""
// CHECK-NEXT: "details": {
// CHECK-NEXT: "swiftPackageProduct": {
// CHECK-NEXT:     "productDescription": "(url: \"https://www.xyz.com/my/other/package.git\", .exact(\"1.2.3\"))"
// CHECK-NEXT:   }
// CHECK-NEXT: }


