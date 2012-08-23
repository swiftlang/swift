// RUN: %swift -repl < %s 2>&1 | FileCheck %s

struct Y { }

class A { 
  func [conversion] __conversion() -> Y {}
}

class B : A { }

struct X { 
  func [conversion] __conversion() -> B {}
}


func fb(_ : B) {}
func fa(_ : A) {}
func fy(_ : Y) {}

var a : A
var b : B
var x : X

// CHECK: Constraints:
// CHECK:   B -> () == $T0 -> $T1
// CHECK:   [byref(heap)] X << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as B
// CHECK:   $T1 as ()
// CHECK:   $T2 as ()
// CHECK:   $T3 as B
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints fb(x)

// CHECK: Constraints:
// CHECK:   A -> () == $T0 -> $T1
// CHECK:   [byref(heap)] X << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as A
// CHECK:   $T1 as ()
// CHECK:   $T2 as ()
// CHECK:   $T3 as B
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints fa(x)

// CHECK: Constraints:
// CHECK:   Y -> () == $T0 -> $T1
// CHECK:   [byref(heap)] B << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as Y
// CHECK:   $T1 as ()
// CHECK:   $T2 as ()
// CHECK:   $T3 as Y
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints fy(b)

// CHECK: Constraints:
// CHECK:   Y -> () == $T0 -> $T1
// CHECK:   [byref(heap)] X << $T0
// CHECK: Type Variables:
// CHECK:   $T0 as Y
// CHECK:   $T1 as ()
// CHECK:   $T2 as ()
// CHECK:   $T3 as B
// CHECK: UNSOLVED
// CHECK: No solution found.
:dump_constraints fy(x)
