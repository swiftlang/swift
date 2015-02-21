// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_while %s | FileCheck %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_while.foo
func foo() -> Int32 {
  var x : Int32 = 0
  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:17 : (0 + 1)
  while (x < 10) {
    x++
  }

  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:18 : (0 + 2)
  while (--x > 0) {
    if (x % 2 == 0) { continue }
  }

  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:18 : ((0 + 4) - 5)
  while (x < 100) {
    if (x++ == 10) { break }
  }

  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:18 : ((0 + 6) - 9)
  while (x < 100) {
    x++
    while (true) { break }
    if (x % 2 == 0) { continue }
    // CHECK: [[@LINE-1]]:33 -> [[@LINE+2]]:4 : (6 - 8)
    if (x > 30) { break }
  }

  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:18 : ((0 + 10) - 11)
  while (x < 100) {
    if (x == 40) { // CHECK: [[@LINE]]:18 -> [[@LINE+2]]:6 : 11
      return x
    }
    ++x
  }

  var y : Int32? = 2
  // CHECK: [[@LINE+1]]:9 -> [[@LINE+1]]:15 : ((0 + 12) - 11)
  while x > 30, let z = y {
    y = nil
  }

  // TODO: [[@LINE+1]]:9 -> [[@LINE+1]]:18 : ((0 + 13) - 11)
  while let z = y {
  }
  // CHECK: [[@LINE-1]]:4 -> [[@LINE+1]]:11 : (0 - 11)
  return x
}

foo()
