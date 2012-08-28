// RUN: %swift -repl < %s 2>&1 | FileCheck %s

struct X {
  var i : Int, j : Int
}

struct Y {
  constructor (x : Int, y : Float, z : String) {}
}

oneof Z {
  char : Char,
  string : String,
  point : (Int, Int)
}

class Base { }
class Derived : Base { }

var d : Derived
typealias Point = (x : Int, y : Int)
var hello : String = "hello";
var world : String = "world";

// CHECK: Constraints:
// CHECK:   $T0 is an integer literal
// CHECK:   $T1 is an integer literal
// CHECK:   ($T0, $T1) <<C Point
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     assuming $T0 == Int64
// CHECK:     assuming $T1 == Int64
// CHECK: Type Variables:
// CHECK:   $T0 as Int64
// CHECK:   $T1 as Int64
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints Point(1, 2)

// CHECK: Constraints:
// CHECK:   [byref(implicit)] Derived <<C Base
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints Base(d)

// CHECK: Constraints:
// CHECK:   $T0 is an integer literal
// CHECK:   $T1 is an integer literal
// CHECK:   ($T0, $T1) <<C X
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     assuming $T0 == Int64
// CHECK:     assuming $T1 == Int64
// CHECK: Type Variables:
// CHECK:   $T0 as Int64
// CHECK:   $T1 as Int64
// CHECK:   $T2 as (i : Int64, j : Int64)
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints X(1, 2)

// CHECK: Constraints:
// CHECK:   $T0 is an integer literal
// CHECK:   $T1 is an integer literal
// CHECK:   $T2 is an ASCII string literal
// CHECK:   ($T0, $T1, $T2) <<C Y
// CHECK: Unresolved overload sets:
// CHECK:   set #0 binds $T3 -> Y:
// CHECK:     Y.constructor: metatype<Y> -> (x : Int, y : Float, z : String) -> Y
// CHECK:     Y.constructor: metatype<Y> -> () -> Y
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #0 for Y.constructor: $T3 -> Y == metatype<Y> -> (x : Int, y : Float, z : String) -> Y
// CHECK:     assuming $T0 == Int64
// CHECK:     assuming $T1 == Float
// CHECK:     assuming $T2 == String
// CHECK: Type Variables:
// CHECK:   $T0 as Int64
// CHECK:   $T1 as Float
// CHECK:   $T2 as String
// CHECK:   $T3 as (x : Int64, y : Float, z : String)
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints Y(1, 2, "hello")

// CHECK: Constraints:
// CHECK:   $T0 is a character literal
// CHECK:   $T0 <<C Z
// CHECK: Unresolved overload sets:
// CHECK:   set #0 binds $T1 -> Z:
// CHECK:     Z.char: metatype<Z> -> Char -> Z
// CHECK:     Z.string: metatype<Z> -> String -> Z
// CHECK:     Z.point: metatype<Z> -> (Int, Int) -> Z
// CHECK: ---Viable constraint systems---
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #0 for Z.char: $T1 -> Z == metatype<Z> -> Ch
// CHECK: ar -> Z
// CHECK:     assuming $T0 == Char
// CHECK: Type Variables:
// CHECK:   $T0 as Char
// CHECK:   $T1 as Char
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints Z('a')

// CHECK: Constraints:
// CHECK:   $T0 is an integer literal
// CHECK:   $T1 is an integer literal
// CHECK:   ($T0, $T1) <<C Z
// CHECK: Unresolved overload sets:
// CHECK:   set #0 binds $T2 -> Z:
// CHECK:     Z.char: metatype<Z> -> Char -> Z
// CHECK:     Z.string: metatype<Z> -> String -> Z
// CHECK:     Z.point: metatype<Z> -> (Int, Int) -> Z
// CHECK: ---Viable constraint systems---
// CHECK: ---Child system #1---
// CHECK: Assumptions:
// CHECK:     selected overload set #0 choice #2 for Z.point: $T2 -> Z == metatype<Z> -> (Int, Int) -> Z
// CHECK:     assuming $T0 == Int64
// CHECK:     assuming $T1 == Int64
// CHECK: Type Variables:
// CHECK:   $T0 as Int64
// CHECK:   $T1 as Int64
// CHECK:   $T2 as (Int64, Int64)
// CHECK: SOLVED (completely)
// CHECK: Unique solution found.
:dump_constraints Z(1, 2)
