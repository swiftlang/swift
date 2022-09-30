// RUN: %target-typecheck-verify-swift -parse-stdlib -debug-constraints > %t.log 2>&1
// RUN: %FileCheck %s < %t.log
import Swift


func takeDoubleAndBool(_: Double, _: Bool) { }

func testTernaryOneWay(b: Bool, b2: Bool) {
  // CHECK: ---Connected components---
  // CHECK-NEXT: 3: $T{{[0-9]+}} depends on 1
  // CHECK-NEXT: 1: $T{{[0-9]+}} $T{{[0-9]+}} $T{{[0-9]+}} depends on 0, 2
  // CHECK-NEXT: 2: $T{{[0-9]+}}
  // CHECK-NEXT: 0: $T{{[0-9]+}}
  // CHECK-NEXT: 4: $T{{[0-9]+}} $T{{[0-9]+}} $T{{[0-9]+}}
  takeDoubleAndBool(
    Builtin.one_way(
      b ? Builtin.one_way(3.14159) : Builtin.one_way(2.71828)),
    b == true)
}

func int8Or16(_ x: Int8) -> Int8 { return x }
func int8Or16(_ x: Int16) -> Int16 { return x }

func testTernaryOneWayOverload(b: Bool) {
  // CHECK: ---Connected components---
  // CHECK: 1: [[A:\$T[0-9]+]] [[B:\$T[0-9]+]] [[C:\$T[0-9]+]] depends on 0, 2
  // CHECK: 2: $T{{[0-9]+}} $T{{[0-9]+}} $T{{[0-9]+}}
  // CHECK: 0: $T{{[0-9]+}} $T{{[0-9]+}} $T{{[0-9]+}}

  // CHECK: Solving component #1
  // CHECK: Attempting type variable [[C]] := Int8

  // CHECK: Solving component #1
  // CHECK: Attempting type variable [[C]] := Int8

  // CHECK: Solving component #1
  // CHECK: Attempting type variable [[C]] := Int8

  // CHECK: Solving component #1
  // CHECK: Attempting type variable [[C]] := Int8
  // CHECK: `-> Considering -> [[A]] conv [[C]]
  // CHECK: `-> Considering -> [[B]] conv [[C]]
  // CHECK: `-> Considering -> [[C]] conv Int8
  // CHECK: <! Found solution: [component: non-default literal(s), value: 2] [component: use of overloaded unapplied function(s), value: 2]>

  // CHECK: <! Composed solution: [component: non-default literal(s), value: 2] [component: use of overloaded unapplied function(s), value: 2]>
  // CHECK-NOT: <! Composed solution: [component: non-default literal(s), value: 2] [component: use of overloaded unapplied function(s), value: 2]>
  let _: Int8 = b ? Builtin.one_way(int8Or16(17)) : Builtin.one_way(int8Or16(42))
}
