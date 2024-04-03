// RUN: %target-sil-opt -enable-sil-verify-all -sil-print-debuginfo -sroa %s | %FileCheck --check-prefix=CHECK-SROA %s
sil_stage canonical

import Builtin
import Swift

struct Empty {}

sil_scope 1 { loc "sroa.swift":7:6 parent @bar : $@convention(thin) (Int64, Int64) -> Int64 }

// CHECK-SROA-LABEL: sil {{.+}} @bar
// bar(in_x:in_y:)
sil hidden @bar : $@convention(thin) (Int64, Int64) -> Int64 {
bb0(%0 : $Int64, %1 : $Int64):
  %4 = alloc_stack $Empty, var, name "my_struct", loc "sroa.swift":8:9, scope 1
  debug_value %4 : $*Empty, let, name "my_copy", expr op_deref, loc "sroa.swift":7:10, scope 1
  // Make sure SROA keeps the debug info
  // CHECK-SROA: debug_value undef : $*Empty, var
  // CHECK-SROA-SAME:        name "my_struct",
  // CHECK-SROA-SAME:        loc "sroa.swift":8:9
  // CHECK-SROA: debug_value undef : $*Empty, let
  // CHECK-SROA-SAME:        name "my_copy",
  // CHECK-SROA-SAME:        loc "sroa.swift":7:10
  %6 = struct $Empty (), loc "sroa.swift":9:8, scope 1
  store %6 to %4 : $*Empty, loc "sroa.swift":10:8, scope 1
  dealloc_stack %4 : $*Empty, loc "sroa.swift":8:9, scope 1
  return %0 : $Int64, loc "sroa.swift":11:5, scope 1
} // end sil function 'foo'
