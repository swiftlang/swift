
// RUN: %empty-directory(%t)
//
// Compile the external swift module.
// RUN: %target-swift-frontend -g -emit-module -enable-resilience \
// RUN:   -emit-module-path=%t/resilient_struct.swiftmodule \
// RUN:   -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
//
// RUN: %target-swift-frontend -g -I %t -emit-ir -enable-resilience %s  -o - \
// RUN:  | %FileCheck %s
import resilient_struct

// Fits in buffer
let small = Size(w: 1, h: 2)

// Needs out-of-line allocation
let large = Rectangle(p: Point(x: 1, y: 2), s: Size(w: 3, h: 4), color: 5)

// CHECK: @"$S17global_resilience5small16resilient_struct4SizeVvp" =
// CHECK-SAME: !dbg ![[SMALL:[0-9]+]]
// CHECK: @"$S17global_resilience5large16resilient_struct9RectangleVvp" =
// CHECK-SAME: !dbg ![[LARGE:[0-9]+]]

// CHECK: ![[SMALL]] = !DIGlobalVariableExpression(
// CHECK-SAME:            expr: !DIExpression())
// CHECK: ![[LARGE]] = !DIGlobalVariableExpression(
// CHECK-SAME:            expr: !DIExpression(DW_OP_deref))
