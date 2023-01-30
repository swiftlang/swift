// RUN: %target-swift-frontend -emit-sil -profile-generate -profile-coverage-mapping  -Xllvm -sil-full-demangle -emit-sorted-sil -module-name coverage_nested %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// https://github.com/apple/swift/issues/61129 – Make sure we don't emit
// duplicate coverage for a nested type.

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_nested13hasNestedTypeyyF" {{.*}} // coverage_nested.hasNestedType() -> ()
// CHECK-NEXT:  [[@LINE+2]]:22 -> [[@LINE+11]]:2 : 0
// CHECK-NEXT:  }
func hasNestedType() {
  struct S {
    // CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_nested13hasNestedTypeyyF1SL_V1iSivpfi" {{.*}}  // variable initialization expression of i : Swift.Int in S #1 in coverage_nested.hasNestedType() -> ()
    // CHECK-NEXT:  [[@LINE+4]]:13 -> [[@LINE+4]]:30 : 0
    // CHECK-NEXT:  [[@LINE+3]]:25 -> [[@LINE+3]]:26 : 1
    // CHECK-NEXT:  [[@LINE+2]]:29 -> [[@LINE+2]]:30 : (0 - 1)
    // CHECK-NEXT:  }
    var i = .random() ? 0 : 1
  }
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_nested.call_auto_closure()
// CHECK-NEXT: [[@LINE+3]]:26 -> [[@LINE+10]]:2 : 0
// CHECK-NEXT: }

func call_auto_closure() {
// CHECK-LABEL: sil_coverage_map {{.*}}// use_auto_closure #1 (@autoclosure () -> Swift.Bool) -> Swift.Bool in coverage_nested.call_auto_closure()
// CHECK-NEXT: [[@LINE+1]]:63 -> [[@LINE+3]]:4 : 0
  func use_auto_closure(_ x: @autoclosure () -> Bool) -> Bool {
    return x() // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  }
  let _ = use_auto_closure(false || true)
}
