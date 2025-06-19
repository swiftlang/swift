// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -scan-dependencies -o %t/deps.json %s -cxx-interoperability-mode=default -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import
// RUN: %validate-json %t/deps.json | %FileCheck %s

import CxxStdlib

// CHECK: "mainModuleName": "deps"

/// --------Main module
// CHECK-LABEL: "modulePath": "deps.swiftmodule",
// CHECK-NEXT: "sourceFiles": [
// CHECK-NEXT: cxx-overlay-underlying-module-lookup.swift
// CHECK-NEXT: ],

// CHECK-NEXT: "directDependencies": [
// CHECK-DAG:     "clang": "CxxShim"
// CHECK-DAG:     "swift": "CxxStdlib"
// CHECK-DAG:     "swift": "Cxx"
// CHECK-DAG:     "swift": "Swift"
// CHECK-DAG:     "swift": "SwiftOnoneSupport"
// CHECK: ],

/// ----------
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}CxxStdlib-{{.*}}.swiftmodule"
// CHECK-NEXT: "sourceFiles": []
// CHECK-NEXT: "directDependencies": [
// CHECK-DAG:     "swift": "Cxx"
// CHECK-DAG:     "swift": "Swift"
// CHECK-DAG:     "clang": "std"
// CHECK-DAG:     "clang": "CxxStdlibShim"
// CHECK: ],
