// RUN: %swift -emit-sil -parse-as-library %s | FileCheck %s

import Builtin

func dup<T>(x : T) -> (T, T) { return (x,x) }
// CHECK:      sil @dup
// CHECK-NEXT: ([[X:%.*]] : *T, [[RESULT:%.*]] : *(T, T)):
// CHECK-NEXT: [[XVAR:%.*]] = alloc_var stack $T
// CHECK-NEXT: copy_addr [[X]] [take] to [[XVAR]] [initialization]
// CHECK-NEXT: [[T0:%.*]] = element_addr [[RESULT]], 0
// CHECK-NEXT: [[T1:%.*]] = element_addr [[RESULT]], 1
// CHECK-NEXT: copy_addr [[XVAR]] to [[T0]] [initialization]
// CHECK-NEXT: copy_addr [[XVAR]] to [[T1]] [initialization]
// CHECK-NEXT: [[T0:%.*]] = tuple ()
// CHECK-NEXT: destroy_addr [[XVAR]]
// CHECK-NEXT: dealloc_var stack [[XVAR]]
// CHECK-NEXT: return ([[T0]])
