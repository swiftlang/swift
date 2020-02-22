// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name IndentedBlock -emit-module-path %t/IndentedBlock.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name IndentedBlock -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/IndentedBlock.symbols.json

/**
   Comment block
   with more indentation.
 */
public struct S5 {}

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 6
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 6
// CHECK-NEXT:   "character": 16
// CHECK: "text": "Comment block"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 7
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 7 
// CHECK-NEXT:   "character": 25
// CHECK: "text": "with more indentation."
