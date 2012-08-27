// RUN: %swift -repl < %s 2>&1 | FileCheck %s

func myMap<T1, T2>(array : T1[], fn : (T1) -> T2) -> T2[] {}

var intArray : Int[]

func toString<T>(x : T) -> String { }

// CHECK: Constraints:
// CHECK:   (x : $T4) -> String == $T5 -> $T6
// CHECK:   [byref(heap)] $T2 << $T5
// CHECK:   $T6 << $T3
// CHECK:   (array : $T0[], fn : $T0 -> $T1) -> $T1[] == $T7 -> $T8
// CHECK:   ([byref(heap)] Int[], ($T2) -> $T3) << $T7
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     assuming $T3 == String
// CHECK:     assuming $T2 == Int64
// CHECK:     assuming $T4 == Int64
// CHECK:     assuming $T1 == String
// CHECK: Type Variables:
// CHECK:   $T0 as Int64
// CHECK:   $T1 as String
// CHECK:   $T2 as Int64
// CHECK:   $T3 as String
// CHECK:   $T4 as Int64
// CHECK:   $T5 as (x : $T4)
// CHECK:   $T6 as String
// CHECK:   $T7 as (array : Slice<$T0>, fn : $T0 -> $T1)
// CHECK:   $T8 as Slice<$T1>
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
// FIXME: Would like to use String() constructor here.
:dump_constraints myMap(intArray, { toString($0) })
