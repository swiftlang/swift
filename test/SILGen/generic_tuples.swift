// RUN: %target-swift-frontend -emit-silgen -parse-as-library %s | FileCheck %s


func dup<T>(let x: T) -> (T, T) { return (x,x) }
// CHECK-LABEL:      sil hidden @_TF14generic_tuples3dup
// CHECK-NEXT: ([[RESULT:%.*]] : $*(T, T), [[XVAR:%.*]] : $*T):
// CHECK-NEXT: debug_value_addr [[XVAR]] : $*T  // let x
// CHECK-NEXT: [[T0:%.*]] = tuple_element_addr [[RESULT]] : {{.*}}, 0
// CHECK-NEXT: [[T1:%.*]] = tuple_element_addr [[RESULT]] : {{.*}}, 1
// CHECK-NEXT: copy_addr [[XVAR]] to [initialization] [[T0]]
// CHECK-NEXT: copy_addr [take] [[XVAR]] to [initialization] [[T1]]
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: return [[T0]]

// <rdar://problem/13822463>
// Specializing a generic function on a tuple type changes the number of
// SIL parameters, which caused a failure in the ownership conventions code.

struct Blub {}
// CHECK-LABEL: sil hidden @_TF14generic_tuples3foo
func foo<T>(x: T) {}
// CHECK-LABEL: sil hidden @_TF14generic_tuples3bar
func bar(x: (Blub, Blub)) { foo(x) }
