// RUN: %swift -repl < %s 2>&1 | FileCheck %s

class A { }
class B : A { }
class C : B { }
class D : B { }

class E<T> : D { }
// FIXME: Can't parse this yet
//class F<T> : E<T[]> { }

var a : A
var b : B
var c : C
var d : D
var ef : E<Float> 
//var fi : F<Int>

func f0(b : B) {}

// CHECK: Constraints:
// CHECK:   (b : B) -> () == $T0 -> $T1
// CHECK:   [byref(heap)] C << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as (b : B)
// CHECK:   $T1 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f0(c)

// CHECK: Constraints:
// CHECK:   (b : B) -> () == $T0 -> $T1
// CHECK:   [byref(heap)] A << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as (b : B)
// CHECK:   $T1 as ()
// CHECK: UNSOLVED
// CHECK: No solution found.
:dump_constraints f0(a)

// CHECK: Constraints:
// CHECK:   (b : B) -> () == $T0 -> $T1
// CHECK:   [byref(heap)] E<Float> << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as (b : B)
// CHECK:   $T1 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f0(ef)
