// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_if_expr %s
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_if_expr %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// CHECK-LABEL: sil_coverage_map {{.*}} "$s16coverage_if_expr0b1_C0SiyF"
func if_expr() -> Int { // CHECK:      [[@LINE]]:23 -> {{[0-9]+}}:2 : 0
  if .random() {        // CHECK-NEXT: [[@LINE]]:6 -> [[@LINE]]:15 : 0
    0                   // CHECK-NEXT: [[@LINE-1]]:16 -> [[@LINE+4]]:4 : 1
                        // CHECK-NEXT: [[@LINE+3]]:4 -> {{[0-9]+}}:2 : 0

                        // FIXME: The below line is incorrect, it should be (0 - 1), but that's an existing bug with else ifs.
  } else if .random() { // CHECK-NEXT: [[@LINE]]:13 -> [[@LINE]]:22 : 0
    1                   // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+1]]:4 : 2
  } else {              // CHECK-NEXT: [[@LINE]]:4 -> {{[0-9]+}}:2 : 0

                        // FIXME: The below line is incorrect, it should be ((0 - 1) - 2), but that's an existing bug with else ifs.
    2                   // CHECK-NEXT: [[@LINE-3]]:10 -> [[@LINE+4]]:4 : (0 - 2)

                        // FIXME: Also incorrect
                        // CHECK-NEXT: [[@LINE+1]]:4 -> [[@LINE+2]]:2 : 0
  }                     // CHECK-NEXT: }
}
