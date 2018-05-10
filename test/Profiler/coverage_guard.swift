// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_guard %s | %FileCheck %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_guard.foo
func foo(_ x : Int32) { // CHECK: [[@LINE]]:23 -> [[END:[0-9]+:2]] : 0
  guard x > 1 else { // CHECK: [[@LINE]]:20 -> [[@LINE+2]]:4 : 1
    return
  } // CHECK: [[@LINE]]:4 -> [[END]] : (0 - 1)

  guard let y : Bool? = x == 3 ? true : nil else { // CHECK: [[@LINE]]:50 ->
    return
  } // CHECK: [[@LINE]]:4 -> [[END]] : ((0 - 1) - 2)

  guard y! else { // CHECK: [[@LINE]]:17 ->
    return
  } // CHECK: [[@LINE]]:4 -> [[END]] : (((0 - 1) - 2) - 4)

  let z = x
}

foo(1);
foo(2);
foo(3);
