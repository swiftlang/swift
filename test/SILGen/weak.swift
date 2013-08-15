// RUN: %swift -emit-silgen %s | FileCheck %s

class C {}

struct A {
  var [weak] x : C
}

// CHECK:    sil @_T4weak5test0FT1cCS_1C_T_ : $[thin] (c : C) -> () {
func test0(c : C) {
// CHECK:    bb0(%0 : $C):
// CHECK:      [[C:%.*]] = alloc_box $C

  var a : A
// CHECK:      [[A:%.*]] = alloc_box $A

  var [weak] x = c
// CHECK:      [[X:%.*]] = alloc_box $[weak] C
// CHECK-NEXT: [[T0:%.*]] = load [[C]]#1 : $*C
// CHECK-NEXT: retain [[T0]] : $C
// CHECK-NEXT: store_weak [[T0]] to [initialization] [[X]]#1 : $*[weak] C
// CHECK-NEXT: release [[T0]] : $C

  a.x = c
// CHECK-NEXT: [[T0:%.*]] = load [[C]]#1 : $*C
// CHECK-NEXT: retain [[T0]] : $C
// CHECK-NEXT: [[T1:%.*]] = struct_element_addr [[A]]#1 : $*A, #x
// CHECK-NEXT: store_weak [[T0]] to [[T1]] : $*[weak] C
// CHECK-NEXT: release [[T0]] : $C

  a.x = x
// CHECK-NEXT: [[T0:%.*]] = load_weak [[X]]#1 : $*[weak] C
// CHECK-NEXT: [[T1:%.*]] = struct_element_addr [[A]]#1 : $*A, #x
// CHECK-NEXT: store_weak [[T0]] to [[T1]] : $*[weak] C
// CHECK-NEXT: release [[T0]] : $C

}
