// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ASCIIArt -emit-module-path %t/ASCIIArt.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name ASCIIArt -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/ASCIIArt.symbols.json

/**
 * With
 * ASCII
 * art.
 */
public struct S6 {}

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 6
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 6
// CHECK-NEXT:   "character": 7
// CHECK: "text": "With"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 7
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 7
// CHECK-NEXT:   "character": 8
// CHECK: "text": "ASCII"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 8
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 8
// CHECK-NEXT:   "character": 7
// CHECK: "text": "art."

