// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DocComment -emit-module-path %t/DocComment.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name DocComment -I %t -pretty-print -o %t/DocComment.symbols.json
// RUN: %FileCheck %s --input-file %t/DocComment.symbols.json

// CHECK: "text": "Single line."

/// Single line.
public struct S1 {}

// CHECK: "text": "Two"
// CHECK: "text": "lines."

/// Two
/// lines.
public struct S2 {}

// CHECK: "text": "There are"
// CHECK: "text": "three lines"
// CHECK: "text": "here."

/// There are
/// three lines
/// here.
public struct S3 {}

// CHECK: "text": "Comment"
// CHECK: "text": "block."

/**
 Comment
 block.
*/
public struct S4 {}

// CHECK: "text": "Comment block"
// CHECK: "text": "with more indentation."

/**
   Comment block
   with more indentation.
 */
public struct S5 {}

// CHECK: "text": "With"
// CHECK: "text": "ASCII"
// CHECK: "text": "art."

/**
 * With
 * ASCII
 * art.
 */
public struct S6 {}
