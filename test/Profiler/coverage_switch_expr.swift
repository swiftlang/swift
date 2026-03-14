// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_switch_expr %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// CHECK-LABEL: sil_coverage_map {{.*}} "$s20coverage_switch_expr0b1_C0ySiSbSgF"
func switch_expr(_ b: Bool?) -> Int { // CHECK-NEXT: [[@LINE]]:37 -> {{[0-9]+}}:2 : 0
  switch b {                          // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE]]:11 : 0
  case true?:                         // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:6 : 1
    0
  case false?:                        // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:6 : 2
    1
  case nil:                           // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:6 : 3
    2                                 // FIXME: The following is incorrect
  }                                   // CHECK-NEXT: [[@LINE]]:4 -> [[@LINE+1]]:2 : ((1 + 2) + 3)
}                                     // CHECK-NEXT: }
