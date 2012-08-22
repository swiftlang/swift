// RUN: %swift -repl < %s 2>&1 | FileCheck %s

func f0(_ : Float) -> Float {}

var f : Float

// Simple call
// CHECK: Constraints:
// CHECK:   Float -> Float == $T0 -> $T1
// CHECK:   [byref(heap)] Float << $T0
// CHECK:   Float -> Float == $T2 -> $T3
// CHECK: Type Variables:
// CHECK:   $T0 as Float
// CHECK:   $T1 as Float
// CHECK:   $T2 as Float
// CHECK:   $T3 as Float
// CHECK: Constraints:
// CHECK-NEXT: SOLVED (completely)
:dump_constraints f0(f0(f))

func f1(x : Float) -> Float {}

// Simple call
// CHECK: Constraints:
// CHECK:  (x : Float) -> Float == $T0 -> $T1
// CHECK:  [byref(heap)] Float << $T0
// CHECK:  (x : Float) -> Float == $T2 -> $T3
// CHECK:  $T1 << $T2
//
// CHECK: Type Variables:
// CHECK:   $T0 as (x : Float)
// CHECK:   $T1 as Float
// CHECK:   $T2 as (x : Float)
// CHECK:   $T3 as Float
// CHECK: Constraints:
// CHECK-NEXT: SOLVED (completely)
:dump_constraints f1(f1(f))
