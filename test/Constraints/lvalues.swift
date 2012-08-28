// RUN: %swift -repl < %s 2>&1 | FileCheck %s

func f0(x : [byref] Int);
func f1<T>(x : [byref] T);

struct X {
  subscript (i : Int) -> Float { get {} set {} }

  var property : Double { get {} set {} }
}

struct Y {
  subscript (i : Int) -> Float { get {} set {} }
  subscript (f : Float) -> Int { get {} set {} }
}

var i : Int
var f : Float
var x : X
var y : Y

// CHECK: Constraints:
// CHECK:   (x : [byref] Int) -> () == $T0 -> $T1
// CHECK:   [byref(heap)] Int << $T0
// CHECK: ---Simplified constraints---
// CHECK: Type Variables:
// CHECK:   $T0 as (x : [byref] Int64)
// CHECK:   $T1 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f0(&i)

// CHECK: Constraints:
// CHECK:   (x : [byref] $T0) -> () == $T1 -> $T2
// CHECK:   [byref(heap)] Int << $T1
// CHECK: ---Simplified constraints---
// CHECK: Type Variables:
// CHECK:   $T0 as Int64
// CHECK:   $T1 as (x : [byref] $T0)
// CHECK:   $T2 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f1(&i)

// CHECK: Constraints:
// CHECK:   [byref(heap)] X[.__subscript: value] == $T1 -> [byref] $T2
// CHECK:   [byref(heap)] Int << $T1
// CHECK:   (x : [byref] $T0) -> () == $T3 -> $T4
// CHECK:   [byref] $T2 << $T3
// CHECK: Type Variables:
// CHECK:   $T0 equivalent to $T2
// CHECK:   $T1 as (i : Int64)
// CHECK:   $T2 as Float
// CHECK:   $T3 as (x : [byref] $T0)
// CHECK:   $T4 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f1(&x[i])

// CHECK: Constraints:
// CHECK:   [byref(heap)] X[.property: value] == $T1
// CHECK:   (x : [byref] $T0) -> () == $T2 -> $T3
// CHECK:   $T1 << $T2
// CHECK: ---Simplified constraints---
// CHECK: Type Variables:
// CHECK:   $T0 as Double
// CHECK:   $T1 as [byref(heap)] Double
// CHECK:   $T2 as (x : [byref] $T0)
// CHECK:   $T3 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f1(&x.property)

// CHECK: Constraints:
// CHECK:   [byref(heap)] Int << $T1
// CHECK: Unresolved overload sets:
// CHECK:   set #0 binds $T1 -> [byref] $T2:
// CHECK:     [byref(heap)] Y.__subscript: (i : Int) -> Float
// CHECK:     [byref(heap)] Y.__subscript: (f : Float) -> Int
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #0 for [byref(heap)] Y.__subscript: $T1 -> [byref] $T2 == (i : Int) -> Float
// CHECK: Type Variables:
// CHECK:   $T0 equivalent to $T2
// CHECK:   $T1 as (i : Int64)
// CHECK:   $T2 as Float
// CHECK:   $T3 as (x : [byref] $T0)
// CHECK:   $T4 as ()
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints f1(&y[i])
