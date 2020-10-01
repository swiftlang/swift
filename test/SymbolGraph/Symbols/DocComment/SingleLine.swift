// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SingleLine -emit-module-path %t/SingleLine.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name SingleLine -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SingleLine.symbols.json

/// Single line.
public struct S1 {}

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 5
// CHECK-NEXT:   "character": 4
// CHECK: end
// CHECK-NEXT:   "line": 5
// CHECK-NEXT:   "character": 16
// CHECK: "text": "Single line."

