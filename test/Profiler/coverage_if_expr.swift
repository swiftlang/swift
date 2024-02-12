// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_if_expr %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// CHECK-LABEL: sil_coverage_map {{.*}} "$s16coverage_if_expr0b1_C0SiyF"
func if_expr() -> Int { // CHECK:      [[@LINE]]:23   -> {{[0-9]+}}:2  : 0
  if .random() {        // CHECK-NEXT: [[@LINE]]:6    -> [[@LINE]]:15  : 0
    0                   // CHECK-NEXT: [[@LINE-1]]:16 -> [[@LINE+1]]:4 : 1
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13   -> [[@LINE]]:22  : (0 - 1)
    1                   // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4 : 2
  } else {              // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+2]]:4 : ((0 - 1) - 2)
    2
  }                     // CHECK-NEXT: }
}
