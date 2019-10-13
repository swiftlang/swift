// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_empty %s | %FileCheck %s

// Skip the sil prologue, which reproduces the swift source of this file
// and confounds FileCheck.
// CHECK-LABEL: sil @main

func singleDefaultArgument(i: Int = {
  // CHECK: sil_coverage_map {{.*}}closure #1 () -> Swift.Int in default argument 0
  // CHECK-NEXT:   [[@LINE-2]]:37 -> [[@LINE+6]]:2 : 0
  // CHECK-NEXT: }
  struct SingleDefaultArgumentStruct {
    let sdasi: Int
  }
  return 2
}()) {
  // CHECK: sil_coverage_map {{.*}}singleDefaultArgument(i: Swift.Int) -> ()
  // CHECK-NEXT:   [[@LINE-2]]:6 -> [[@LINE+3]]:2 : 0
  // CHECK-NEXT: }
  print(i)
}
