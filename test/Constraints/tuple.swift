// Test various tuple constraints.

// RUN: %swift -repl < %s 2>&1 | FileCheck %s

func f0(x : Int, y : Float) {}

var i : Int
var f : Float

func f1(y : Float, rest : Int...) {}

//===--------------------------------------------------------------------===//
// Conversions and shuffles
//===--------------------------------------------------------------------===//

// Basic tuple shuffle
// CHECK: Constraints:
// CHECK:   $T0 is an integer literal
// CHECK:   (x : Int, y : Float) -> () == $T1 -> $T2
// CHECK:   (y : [byref(heap)] Float, x : $T0) << $T1
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     assuming $T0 == Int64
// CHECK: Type Variables:
// CHECK:   $T0 as Int64
// CHECK:   $T1 as (x : Int64, y : Float)
// CHECK:   $T2 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f0(y=f, x=1)

// Variadic functions.
// CHECK: Constraints:
// CHECK:   $T0 is an integer literal
// CHECK:   $T1 is an integer literal
// CHECK:   $T2 is an integer literal
// CHECK:   $T3 is an integer literal
// CHECK:   $T4 is an integer literal
// CHECK:   (y : Float, rest : Int...) -> () == $T5 -> $T6
// CHECK:   (y : [byref(heap)] Float, $T0, $T1, $T2, $T3, $T4) << $T5
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     assuming $T0 == Int64
// CHECK:     assuming $T1 == Int64
// CHECK:     assuming $T2 == Int64
// CHECK:     assuming $T3 == Int64
// CHECK:     assuming $T4 == Int64
// CHECK: Type Variables:
// CHECK:   $T0 as Int64
// CHECK:   $T1 as Int64
// CHECK:   $T2 as Int64
// CHECK:   $T3 as Int64
// CHECK:   $T4 as Int64
// CHECK:   $T5 as (y : Float, rest : Int64...)
// CHECK:   $T6 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
// FIXME: IRgen can't handle y=f at the end.
:dump_constraints f1(y=f, 1, 2, 3, 4, 5)
