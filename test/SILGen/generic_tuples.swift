// RUN: %swift -emit-silgen -parse-as-library %s | FileCheck %s


func dup<T>(x : T) -> (T, T) { return (x,x) }
// CHECK:      sil @_T14generic_tuples3dupU__FT1xQ__TQ_Q__
// CHECK-NEXT: ([[RESULT:%.*]] : $*(T, T), [[XVAR:%.*]] : $*T):
// CHECK-NEXT: [[BOX:%.*]] = alloc_box
// CHECK-NEXT:  copy_addr
// CHECK-NEXT: [[T0:%.*]] = tuple_element_addr [[RESULT]] : {{.*}}, 0
// CHECK-NEXT: [[T1:%.*]] = tuple_element_addr [[RESULT]] : {{.*}}, 1
// CHECK-NEXT: copy_addr [[BOX]]#1 to [initialization] [[T0]]
// CHECK-NEXT: copy_addr [[BOX]]#1 to [initialization] [[T1]]
// CHECK-NEXT: release [[BOX]]
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: return [[T0]]

// <rdar://problem/13822463>
// Specializing a generic function on a tuple type changes the number of
// SIL parameters, which caused a failure in the ownership conventions code.

struct Blub {}
// CHECK: sil @_T14generic_tuples3fooU__FT1xQ__T_
func foo<T>(x:T) {}
// CHECK: sil @_T14generic_tuples3barFT1xTVS_4BlubS0___T_
func bar(x:(Blub, Blub)) { foo(x) }
