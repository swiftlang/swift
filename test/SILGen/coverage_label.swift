// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_label %s | %FileCheck %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_label.foo
func foo() { // CHECK-DAG: [[@LINE]]:12 -> [[@LINE+19]]:2 : 0
  var x : Int32 = 0

  label1: do { // CHECK-DAG: [[@LINE]]:14 -> [[@LINE+4]]:4 : 0
    x += 1
    break label1
    x += 2     // CHECK-DAG: [[@LINE]]:5 -> [[@LINE+1]]:4 : zero
  }

  label2: do { // CHECK-DAG: [[@LINE]]:14 -> [[@LINE+7]]:4 : 0
    x += 3         // CHECK-DAG: [[@LINE+1]]:11 -> [[@LINE+1]]:17 : 0
    while (true) { // CHECK-DAG: [[@LINE]]:18 -> [[@LINE+3]]:6 : 1
      x += 4
      break label2 // Note: This exit affects the condition counter expr @ L15.
    }              // CHECK-DAG: [[@LINE]]:6 -> [[@LINE+2]]:4 : (0 - 1)
    x += 5
  }

  x += 6
}

foo()
