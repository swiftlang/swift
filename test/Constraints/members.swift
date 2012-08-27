// RUN: %swift -repl < %s 2>&1 | FileCheck %s

struct X {
  func f0(i : Int) -> X { }

  func f1(i : Int) { }
  func f1(f : Float) { }

  func f2<T>(x : T) -> T { }
}

var i : Int
var x : X

func g0(_ : ([byref] X) -> (Float) -> ()) {}

// Simple member function access
// CHECK: Constraints:
// CHECK:   [byref(heap)] X[.f0: value] == $T0
// CHECK:   $T0 == $T1 -> $T2
// CHECK:   [byref(heap)] Int << $T1
// CHECK: Type Variables:
// CHECK:   $T0 as $T1 -> $T2
// CHECK:   $T1 as (i : Int64)
// CHECK:   $T2 as X
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints x.f0(i)

// Overloaded member function access
// CHECK: Constraints:
// CHECK:   [byref(heap)] X[.f0: value] == $T0
// CHECK:   $T0 == $T1 -> $T2
// CHECK:   [byref(heap)] Int << $T1
// CHECK:   $T2[.f1: value] == $T3
// CHECK:   $T3 == $T4 -> $T5
// CHECK:   [byref(heap)] Int << $T4
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #0 for X.f1: $T3 == [byref] X -> (i : Int) -> ()
// CHECK: Type Variables:
// CHECK:   $T0 as $T1 -> $T2
// CHECK:   $T1 as (i : Int64)
// CHECK:   $T2 as X
// CHECK:   $T3 as $T4 -> $T5
// CHECK:   $T4 as (i : Int64)
// CHECK:   $T5 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints x.f0(i).f1(i)

// CHECK: Constraints:
// CHECK:   metatype<X>[.f1: value] == $T0
// CHECK:   ([byref] X) -> (Float) -> () -> () == $T1 -> $T2
// CHECK:   $T0 << $T1
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #1 for metatype<X>.f1: $T0 == [byref] X -> (f : Float) -> ()
// CHECK: Type Variables:
// CHECK:   $T0 as [byref] X -> (f : Float) -> ()
// CHECK:   $T1 as [byref] X -> Float -> ()
// CHECK:   $T2 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints g0(X.f1)
