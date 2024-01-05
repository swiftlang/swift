// RUN: %target-swift-frontend -parse-as-library -emit-silgen -profile-generate %s | %FileCheck %s

// CHECK: sil hidden [ossa] @[[F_EMPTY:.*empty.*]] :
// CHECK: increment_profiler_counter 0, "{{.*}}instrprof_basic.swift:[[F_EMPTY]]", num_counters 1, hash
func empty() {
  // CHECK-NOT: increment_profiler_counter
}

// CHECK: sil hidden [ossa] @[[F_BASIC:.*basic.*]] :
// CHECK: increment_profiler_counter 0, "{{.*}}instrprof_basic.swift:[[F_BASIC]]", num_counters 6, hash
func basic(a : Int32) {

  // CHECK: increment_profiler_counter
  if a == 0 {
  }

  // CHECK: increment_profiler_counter
  if a != 0 {
  } else {
  }

  // CHECK: increment_profiler_counter
  while a == 0 {
  }

  // CHECK: increment_profiler_counter
  for i in 0 ..< a {
  }

  // CHECK: increment_profiler_counter
  for i in 1...a {
  }

  // CHECK-NOT: increment_profiler_counter
}

// CHECK: sil hidden [ossa] @[[F_THROWING_NOP:.*throwing_nop.*]] :
func throwing_nop() throws {}
// CHECK: sil hidden [ossa] @[[F_EXCEPTIONS:.*exceptions.*]] :
// CHECK: increment_profiler_counter 0, "{{.*}}instrprof_basic.swift:[[F_EXCEPTIONS]]", num_counters 3, hash
func exceptions() {
  do {
    // CHECK: increment_profiler_counter
    try throwing_nop()
  } catch {
    // CHECK: increment_profiler_counter
    return
  }

  // CHECK-NOT: increment_profiler_counter
}
