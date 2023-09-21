// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -scan-dependencies %s -o %t/deps.json -emit-dependencies -emit-dependencies-path %t/deps.d -swift-version 4 -Xcc -Xclang
// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json | %FileCheck %s

// Ensure that round-trip serialization does not affect result
// RUN: %target-swift-frontend -scan-dependencies -test-dependency-scan-cache-serialization %s -o %t/deps.json -emit-dependencies -emit-dependencies-path %t/deps.d -swift-version 4 -Xcc -Xclang
// RUN: %validate-json %t/deps.json | %FileCheck %s

// REQUIRES: OS=macosx

import CryptoKit

// CHECK: "mainModuleName": "deps"
// CHECK: directDependencies
// CHECK-DAG: "swift": "CryptoKit"
// CHECK-DAG: "swift": "Swift"
// CHECK-DAG: "swift": "SwiftOnoneSupport"
// CHECK-DAG: "swift": "_Concurrency"
// CHECK-DAG: "swift": "_StringProcessing"
// CHECK: ],

// CHECK: "isFramework": true
