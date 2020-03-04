// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DocComment -emit-module-path %t/DocComment.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name DocComment -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/DocComment.symbols.json "-DTESTFILE=%s"

/// Single line.
public struct S1 {}

/// Two
/// lines.
public struct S2 {}

/// There are
/// three lines
/// here.
public struct S3 {}

/**
 Comment
 block.
*/
public struct S4 {}

/**
   Comment block
   with more indentation.
 */
public struct S5 {}

/**
 * With
 * ASCII
 * art.
 */
public struct S6 {}

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 5
// CHECK-NEXT:   "character": 4
// CHECK: end
// CHECK-NEXT:   "line": 5
// CHECK-NEXT:   "character": 16
// CHECK: "text": "Single line."

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 8
// CHECK-NEXT:   "character": 4
// CHECK: end
// CHECK-NEXT:   "line": 8
// CHECK-NEXT:   "character": 7
// CHECK: "text": "Two"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 9
// CHECK-NEXT:   "character": 4
// CHECK: end
// CHECK-NEXT:   "line": 9
// CHECK-NEXT:   "character": 10
// CHECK: "text": "lines."

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 12
// CHECK-NEXT:   "character": 4
// CHECK: end
// CHECK-NEXT:   "line": 12
// CHECK-NEXT:   "character": 13
// CHECK: "text": "There are"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 13
// CHECK-NEXT:   "character": 4
// CHECK: end
// CHECK-NEXT:   "line": 13
// CHECK-NEXT:   "character": 15
// CHECK: "text": "three lines"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 14
// CHECK-NEXT:   "character": 4
// CHECK: end
// CHECK-NEXT:   "line": 14
// CHECK-NEXT:   "character": 9
// CHECK: "text": "here."

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 18
// CHECK-NEXT:   "character": 1
// CHECK: end
// CHECK-NEXT:   "line": 18
// CHECK-NEXT:   "character": 8
// CHECK: "text": "Comment"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 19
// CHECK-NEXT:   "character": 1
// CHECK: end
// CHECK-NEXT:   "line": 19
// CHECK-NEXT:   "character": 7
// CHECK: "text": "block."

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 24
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 24
// CHECK-NEXT:   "character": 16
// CHECK: "text": "Comment block"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 25
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 25
// CHECK-NEXT:   "character": 25
// CHECK: "text": "with more indentation."

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 26
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 26
// CHECK-NEXT:   "character": 3

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 30
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 30
// CHECK-NEXT:   "character": 7
// CHECK: "text": "With"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 31
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 31
// CHECK-NEXT:   "character": 8
// CHECK: "text": "ASCII"

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 32
// CHECK-NEXT:   "character": 3
// CHECK: end
// CHECK-NEXT:   "line": 32
// CHECK-NEXT:   "character": 7
// CHECK: "text": "art."

// CHECK: range
// CHECK-NEXT: start
// CHECK-NEXT:   "line": 33
// CHECK-NEXT:   "character": 0
// CHECK: end
// CHECK-NEXT:   "line": 33
// CHECK-NEXT:   "character": 1
