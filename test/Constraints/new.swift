// RUN: %swift -repl < %s 2>&1 | FileCheck %s

struct X<T> { }
var i : Int
func acceptXIntArray(xia : X<Int>[]) {}

// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints new Int[i]

// CHECK: Constraints:
// CHECK:   (xia : X<Int>[]) -> () == $T1 -> $T2
// CHECK:   X<$T0>[] << $T1
// CHECK: ---Simplified constraints---
// CHECK: Type Variables:
// CHECK:   $T0 as Int64
// CHECK:   $T1 as (xia : Slice<X<Int64>>)
// CHECK:   $T2 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints acceptXIntArray(new X[i])

