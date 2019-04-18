
// RUN: %empty-directory(%t)
//
// Compile the external swift module.
// RUN: %target-swift-frontend -g -emit-module -enable-library-evolution \
// RUN:   -emit-module-path=%t/resilient_struct.swiftmodule \
// RUN:   -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
//
// RUN: %target-swift-frontend -g -I %t -emit-ir -enable-library-evolution %s  -o - \
// RUN:  | %FileCheck %s
import resilient_struct

// Fits in buffer
let small = Size(w: 1, h: 2)

// Needs out-of-line allocation
let large = Rectangle(p: Point(x: 1, y: 2), s: Size(w: 3, h: 4), color: 5)

// CHECK: @"$s17global_resilience5small16resilient_struct4SizeVvp" =
// CHECK-SAME: !dbg ![[SMALL:[0-9]+]]
// CHECK: @"$s17global_resilience5large16resilient_struct9RectangleVvp" =
// CHECK-SAME: !dbg ![[LARGE:[0-9]+]]

// CHECK: ![[SMALL]] = !DIGlobalVariableExpression(
// CHECK-SAME:            var: ![[VAR_SMALL:[0-9]+]]
// CHECK-SAME:            expr: !DIExpression())
// CHECK: ![[VAR_SMALL]] = distinct !DIGlobalVariable(
// CHECK-SAME:            type: ![[SMALL_TY:[0-9]+]]
// CHECK: ![[SMALL_TY]] = !DICompositeType(tag: DW_TAG_structure_type,
// CHECK-SAME:            name: "$swift.fixedbuffer", 
// CHECK: ![[LARGE]] = !DIGlobalVariableExpression(
// CHECK-SAME:            var: ![[VAR_LARGE:[0-9]+]]
// CHECK-SAME:            expr: !DIExpression())
// CHECK: ![[VAR_LARGE]] = distinct !DIGlobalVariable(
// CHECK-SAME:            type: ![[LARGE_TY:[0-9]+]]
// CHECK: ![[LARGE_TY]] = !DICompositeType(tag: DW_TAG_structure_type,
// CHECK-SAME:            name: "$swift.fixedbuffer", 
