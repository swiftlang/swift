// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_closures %s | FileCheck %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_closures.foo
func foo() {
  // CHECK: [[@LINE+1]]:12 -> [[@LINE+1]]:59 : 1
  let c1 = { (i1 : Int32, i2 : Int32) -> Bool in i1 < i2 }

  func f1(closure : (Int32, Int32) -> Bool) -> Bool {
    // CHECK: [[@LINE-1]]:53 -> [[@LINE+3]]:4 : 2
    // CHECK: [[@LINE+1]]:29 -> [[@LINE+1]]:42 : 3
    return closure(0, 1) && closure(1, 0)
  }

  f1(c1)
  // CHECK: [[@LINE+1]]:6 -> [[@LINE+1]]:27 : 4
  f1 { i1, i2 in i1 > i2 }
  // CHECK: [[@LINE+2]]:6 -> [[@LINE+2]]:48 : 5
  // CHECK: [[@LINE+1]]:36 -> [[@LINE+1]]:46 : 6
  f1 { left, right in left == 0 || right == 1 }
}

foo()
