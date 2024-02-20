// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_do %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// CHECK-LABEL: sil hidden @$s11coverage_do3fooyyF : $@convention(thin) () -> ()

// CHECK:       increment_profiler_counter 0
// CHECK:       function_ref @$sSb6randomSbyFZ
// CHECK:       cond_br {{%[0-9]+}}, [[EXITBB:bb[0-9]]], [[BB1:bb[0-9]]]

// CHECK:       [[BB1]]
// CHECK:       function_ref @$sSb6randomSbyFZ
// CHECK:       cond_br {{%[0-9]+}}, [[BRKBB:bb[0-9]]], {{bb[0-9]}}

// CHECK:       [[BRKBB]]
// CHECK-NEXT:  increment_profiler_counter 2

// CHECK:       [[EXITBB]]
// CHECK-NEXT:  increment_profiler_counter 1

// CHECK-LABEL: sil_coverage_map {{.*}} "$s11coverage_do3fooyyF"
func foo() {       // CHECK-NEXT: [[@LINE]]:12   -> [[@LINE+11]]:2 : 0
  x: do {          // CHECK-NEXT: [[@LINE]]:9    -> [[@LINE+8]]:4  : 0
    if .random() { // CHECK-NEXT: [[@LINE]]:8    -> [[@LINE]]:17   : 0
      return       // CHECK-NEXT: [[@LINE-1]]:18 -> [[@LINE+1]]:6  : 1
    }              // CHECK-NEXT: [[@LINE]]:6    -> [[@LINE+4]]:11 : (0 - 1)
    if .random() { // CHECK-NEXT: [[@LINE]]:8    -> [[@LINE]]:17   : (0 - 1)
      break x      // CHECK-NEXT: [[@LINE-1]]:18 -> [[@LINE+1]]:6  : 2
    }              // CHECK-NEXT: [[@LINE]]:6    -> [[@LINE+1]]:11 : ((0 - 1) - 2)
    return
  }                // CHECK-NEXT: [[@LINE]]:4   -> [[@LINE+2]]:2 : 2
  do {}            // CHECK-NEXT: [[@LINE]]:6   -> [[@LINE]]:8   : 2
}                  // CHECK-NEXT: [[@LINE-1]]:8 -> [[@LINE]]:2   : 2
                   // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s11coverage_do4foobyyF"
func foob() {
  x: do {
    do {
      if .random() { return }
      // CHECK: [[@LINE+1]]:6 -> [[@LINE+10]]:4 : (0 - 1)
    }
    do {
      if .random() { break x }
      // CHECK: [[@LINE+1]]:6 -> [[@LINE+6]]:4 : ((0 - 1) - 2)
    }
    do {
      return
      // CHECK-NOT: zero
    }
  }
  do {}
}
