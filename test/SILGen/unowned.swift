// RUN: %swift -emit-silgen %s | FileCheck %s

class C {}

struct A {
  var [unowned] x : C
}

// CHECK:    sil @_T7unowned5test0FT1cCS_1C_T_ : $[thin] (c : C) -> () {
func test0(c : C) {
// CHECK:    bb0(%0 : $C):
// CHECK:      [[C:%.*]] = alloc_box $C

  var a : A
// CHECK:      [[A:%.*]] = alloc_box $A

  var [unowned] x = c
// CHECK:      [[X:%.*]] = alloc_box $[unowned] C
// CHECK-NEXT: [[T0:%.*]] = load [[C]]#1 : $*C
// CHECK-NEXT: retain [[T0]] : $C
// CHECK-NEXT: [[T1:%.*]] = ref_to_unowned [[T0]] : $C  to $[unowned] C
// CHECK-NEXT: unowned_retain [[T1]] : $[unowned] C
// CHECK-NEXT: store [[T1]] to [[X]]#1 : $*[unowned] C
// CHECK-NEXT: release [[T0]] : $C

  a.x = c
// CHECK-NEXT: [[T0:%.*]] = load [[C]]#1 : $*C
// CHECK-NEXT: retain [[T0]] : $C
// CHECK-NEXT: [[T1:%.*]] = struct_element_addr [[A]]#1 : $*A, #x
// CHECK-NEXT: [[T3:%.*]] = ref_to_unowned [[T0]] : $C to $[unowned] C
// CHECK-NEXT: [[T2:%.*]] = load [[T1]] : $*[unowned] C
// CHECK-NEXT: unowned_retain [[T3]] : $[unowned] C
// CHECK-NEXT: store [[T3]] to [[T1]] : $*[unowned] C
// CHECK-NEXT: release [[T0]] : $C
// CHECK-NEXT: unowned_release [[T2]] : $[unowned] C

  a.x = x
// CHECK-NEXT: [[T0:%.*]] = load [[X]]#1 : $*[unowned] C
// CHECK-NEXT: retain_unowned [[T0]] : $[unowned] C
// CHECK-NEXT: [[T1:%.*]] = unowned_to_ref [[T0]] : $[unowned] C to $C
// CHECK-NEXT: [[T2:%.*]] = struct_element_addr [[A]]#1 : $*A, #x
// CHECK-NEXT: [[T4:%.*]] = ref_to_unowned [[T1]] : $C to $[unowned] C
// CHECK-NEXT: [[T3:%.*]] = load [[T2]] : $*[unowned] C
// CHECK-NEXT: unowned_retain [[T4]] : $[unowned] C
// CHECK-NEXT: store [[T4]] to [[T2]] : $*[unowned] C
// CHECK-NEXT: release [[T1]] : $C
// CHECK-NEXT: unowned_release [[T3]] : $[unowned] C

}
