// RUN: %swift -parse -verify -constraint-checker -debug-constraints %s > %t
// RUN: FileCheck %s < %t

func f0<T>(x : T) {}

// FIXME: Lookup breaks if these come after f1!
class A { };
class B : A { }

func f1(a : A) -> A { return a }
func f1(b : B) -> B { return b }

// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #0 for f1: $T1 == (a : A) -> A
// CHECK:     assuming $T0 == A

// CHECK: Type Variables:
// CHECK:   $T0 as A
// CHECK:   $T1 as $T3 -> $T4
// CHECK:   $T2 as ()
// CHECK:   $T3 as (a : A)
// CHECK:   $T4 as A
// CHECK:   $T5 as (x : $T0)
// CHECK:   $T6 as ()
// CHECK: SOLVED (completely)
// CHECK: ---Child system #2 (best)---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #1 for f1: $T1 == (b : B) -> B
// CHECK:     assuming $T0 == B

// CHECK: Type Variables:
// CHECK:   $T0 as B
// CHECK:   $T1 as $T3 -> $T4
// CHECK:   $T2 as ()
// CHECK:   $T3 as (b : B)
// CHECK:   $T4 as B
// CHECK:   $T5 as (x : $T0)
// CHECK:   $T6 as ()
// CHECK: SOLVED (completely)
// CHECK: SOLVED (completely)
f0(f1(new B()))
