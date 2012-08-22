// RUN: %swift -repl < %s 2>&1 | FileCheck %s

func f0(_ : Float) -> Float {}
func f0(_ : Int) -> Int {}

func f1(_ : Int) {}

var i : Int

// Overload resolution based on argument type.
// CHECK: Constraints
// CHECK:  $T0 == $T1 -> $T2
// CHECK:  [byref(heap)] Int << $T1
// CHECK: Unresolved overload sets:
// CHECK:   set #0 binds $T0:
// CHECK:     f0: Float -> Float
// CHECK:     f0: Int -> Int
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:    selected overload set #0 choice #1 for f0: $T0 == Int -> Int
// CHECK: Type Variables:
// CHECK:   $T0 as $T1 -> $T2
// CHECK:   $T1 as Int64
// CHECK:   $T2 as Int64
// CHECK: SOLVED (completely)
:dump_constraints(f0(i))

// Overload resolution based on literalness
// CHECK: Constraints:
// CHECK:   $T1 is an float literal
// CHECK:   $T0 == $T2 -> $T3
// CHECK:   $T1 << $T2
// CHECK: Unresolved overload sets:
// CHECK:   set #0 binds $T0:
// CHECK:     f0: Float -> Float
// CHECK:     f0: Int -> Int
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #0 for f0: $T0 == Float -> Float
// CHECK:     assuming $T2 == Float
// CHECK: Type Variables:
// CHECK:   $T0 as $T2 -> $T3
// CHECK:   $T1 as Float
// CHECK:   $T2 as Float
// CHECK:   $T3 as Float
// CHECK: SOLVED (completely)
:dump_constraints(f0(1.0))

// Failed resolution based on literalness
// FIXME: This will eventually resolve based on the 'default' integer literal
// FIXME: type.
// CHECK: Constraints:
// CHECK:   $T1 is an integer literal
// CHECK:   $T0 == $T2 -> $T3
// CHECK:   $T1 << $T2
// CHECK: Unresolved overload sets:
// CHECK:   set #0 binds $T0:
// CHECK:     f0: Float -> Float
// CHECK:     f0: Int -> Int
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #0 for f0: $T0 == Float -> Float
// CHECK:     assuming $T2 == Float
// CHECK: Type Variables:
// CHECK:   $T0 as $T2 -> $T3
// CHECK:   $T1 as Float
// CHECK:   $T2 as Float
// CHECK:   $T3 as Float
// CHECK: SOLVED (completely)
// CHECK: ---Child system #2---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #1 for f0: $T0 == Int -> Int
// CHECK:     assuming $T2 == Int64
// CHECK: Type Variables:
// CHECK:   $T0 as $T2 -> $T3
// CHECK:   $T1 as Int64
// CHECK:   $T2 as Int64
// CHECK:   $T3 as Int64
// CHECK: SOLVED (completely)
:dump_constraints(f0(1))


