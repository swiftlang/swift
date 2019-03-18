// REQUIRES: syntax_parser_lib
// RUN: %swift-syntax-parser-test %s -dump-tree | %FileCheck %s

let x: a[i] & b
// CHECK: |x|

let x2: a & b[1]
// CHECK: |x2|
