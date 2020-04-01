// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Block -emit-module-path %t/Block.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name Block -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Block.symbols.json

/**
 Comment
 block.
*/
public struct S4 {}

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 6
// CHECK-NEXT:   "character": 1
// CHECK: end
// CHECK-NEXT:   "line": 6
// CHECK-NEXT:   "character": 8
// CHECK: "text": "Comment"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 7
// CHECK-NEXT:   "character": 1
// CHECK: end
// CHECK-NEXT:   "line": 7
// CHECK-NEXT:   "character": 7
// CHECK: "text": "block."

