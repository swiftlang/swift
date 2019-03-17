// REQUIRES: syntax_parser_lib
// RUN: %swift-syntax-parser-test %s -dump-tree | %FileCheck %s

let x: a[i] & b
// CHECK: |let|
