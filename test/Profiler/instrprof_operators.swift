// RUN: %target-swift-frontend -parse-as-library -emit-silgen -profile-generate %s | %FileCheck %s

// CHECK: sil hidden [ossa] @[[F_OPERATORS:.*operators.*]] :
// CHECK: increment_profiler_counter 0, "{{.*}}instrprof_operators.swift:[[F_OPERATORS]]", num_counters 2, hash
func operators(a : Bool, b : Bool) {
  let c = a && b
  let d = a || b

  // CHECK: increment_profiler_counter 1, "{{.*}}instrprof_operators.swift:[[F_OPERATORS]]", num_counters 2, hash
  let e = c ? a : b

  // CHECK-NOT: increment_profiler_counter
}

// CHECK: implicit closure
// CHECK: increment_profiler_counter 0, "{{.*}}:$s19instrprof_operators0B01a1bySb_SbtFSbyKXEfu_", num_counters 1, hash
// CHECK-NOT: increment_profiler_counter

// CHECK: implicit closure
// CHECK: increment_profiler_counter 0, "{{.*}}:$s19instrprof_operators0B01a1bySb_SbtFSbyKXEfu0_", num_counters 1, hash
// CHECK-NOT: increment_profiler_counter
