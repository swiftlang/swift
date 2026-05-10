// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Basic -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Basic -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Basic.symbols.json

// REQUIRES: OS=macosx

@available(macOS, introduced: 10.9, deprecated: 11.0, obsoleted: 15.1, message: "Everyone makes mistakes", renamed: "S2")
public struct S {}

// CHECK: "domain": "macOS"

// CHECK: introduced
// CHECK-NEXT: "major": 10
// CHECK-NEXT: "minor": 9

// CHECK: deprecated 
// CHECK-NEXT: "major": 11
// CHECK-NEXT: "minor": 0

// CHECK: obsoleted
// CHECK-NEXT: "major": 15
// CHECK-NEXT: "minor": 1

// CHECK: "message": "Everyone makes mistakes"
// CHECK: "renamed": "S2"
