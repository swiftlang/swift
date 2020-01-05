// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift
// RUN: %FileCheck %s < %t/deps.json

import C
import E

// CHECK: "mainModuleName": "deps"

/// --------Main module
// CHECK-LABEL: "modulePath": "deps.swiftmodule",
// CHECK-NEXT: sourceFiles
// CHECK-NEXT: module_deps.swift

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "C"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "E"

/// --------Clang module C
// CHECK-LABEL: "modulePath": "C.pcm",

// CHECK: "sourceFiles": [
// CHECK-NEXT: module.modulemap
// CHECK-NEXT: C.h

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "B"

// CHECK: "moduleMapPath"
// CHECK-SAME: module.modulemap

/// --------Swift module E
// CHECK: "swift": "E"
// CHECK-LABEL: modulePath": "E.swiftmodule"
// CHECK: "directDependencies"
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "Swift"

// CHECK: "moduleInterfacePath"
// CHECK-SAME: E.swiftinterface

/// --------Clang module B
// CHECK-LABEL: "modulePath": "B.pcm"

// CHECK-NEXT: sourceFiles
// CHECK-NEXT: B.h
// CHECK-NEXT: module.modulemap

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "A"
// CHECK-NEXT: }

/// --------Swift module Swift
// CHECK-LABEL: "modulePath": "Swift.swiftmodule",

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "SwiftShims"

/// --------Swift module A
// CHECK-LABEL: "modulePath": "A.swiftmodule",

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "A"
// CHECK-NEXT: }

/// --------Clang module SwiftShims
// CHECK-LABEL: "modulePath": "SwiftShims.pcm",
