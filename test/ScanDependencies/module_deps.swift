// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -emit-dependencies -emit-dependencies-path %t/deps.d

// RUN: %FileCheck %s < %t/deps.json
// RUN: %FileCheck %s -check-prefix CHECK-MAKE-DEPS < %t/deps.d

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

/// --------Swift module A
// CHECK-LABEL: "modulePath": "A.swiftmodule",

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "A"
// CHECK-NEXT: }

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

/// --------Clang module SwiftShims
// CHECK-LABEL: "modulePath": "SwiftShims.pcm",


// Check make-style dependencies
// CHECK-MAKE-DEPS: module_deps.swift
// CHECK-MAKE-DEPS-SAME: Swift.swiftmodule
// CHECK-MAKE-DEPS-SAME: A.swiftinterface
// CHECK-MAKE-DEPS-SAME: B.h
// CHECK-MAKE-DEPS-SAME: module.modulemap
