// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name MultiSingleLine -emit-module-path %t/MultiSingleLine.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name MultiSingleLine -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/MultiSingleLine.symbols.json

/// Two
/// lines.
public struct S2 {}

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 5
// CHECK-NEXT:   "character": 4
// CHECK: end
// CHECK-NEXT:   "line": 5
// CHECK-NEXT:   "character": 7
// CHECK: "text": "Two"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 6
// CHECK-NEXT:   "character": 4
// CHECK: end
// CHECK-NEXT:   "line": 6
// CHECK-NEXT:   "character": 10
// CHECK: "text": "lines."


