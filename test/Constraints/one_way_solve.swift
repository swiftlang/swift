// RUN: %target-typecheck-verify-swift -parse-stdlib -debug-constraints > %t.log 2>&1
// RUN: %FileCheck %s < %t.log
import Swift


func takeDoubleAndBool(_: Double, _: Bool) { }

func testTernaryOneWay(b: Bool, b2: Bool) {
  // CHECK: ---Connected components---
  // CHECK-NEXT: 3: $T10 depends on 1
  // CHECK-NEXT: 1: $T5 $T8 $T9 depends on 0, 2
  // CHECK-NEXT: 2: $T7
  // CHECK-NEXT: 0: $T4
  // CHECK-NEXT: 4: $T11 $T13 $T14
  takeDoubleAndBool(
    Builtin.one_way(
      b ? Builtin.one_way(3.14159) : Builtin.one_way(2.71828)),
    b == true)
}

func int8Or16(_ x: Int8) -> Int8 { return x }
func int8Or16(_ x: Int16) -> Int16 { return x }

func testTernaryOneWayOverload(b: Bool) {
  // CHECK: ---Connected components---
  // CHECK: 1: $T5 $T10 $T11 depends on 0, 2
  // CHECK: 2: $T7 $T8 $T9
  // CHECK: 0: $T2 $T3 $T4

  // CHECK: solving component #1
  // CHECK: Initial bindings: $T11 := Int16, $T11 := Int8

  // CHECK: solving component #1
  // CHECK: Initial bindings: $T11 := Int16, $T11 := Int8

  // CHECK: solving component #1
  // CHECK: Initial bindings: $T11 := Int8, $T11 := Int16

  // CHECK: solving component #1
  // CHECK: Initial bindings: $T11 := Int8
  // CHECK: found solution 0 0 0 0 0 0 2 0 0 0 0 0

  // CHECK: composed solution 0 0 0 0 0 0 2 0 0 0 0 0
  // CHECK-NOT: composed solution 0 0 0 0 0 0 2 0 0 0 0 0
  let _: Int8 = b ? Builtin.one_way(int8Or16(17)) : Builtin.one_way(int8Or16(42))
}
