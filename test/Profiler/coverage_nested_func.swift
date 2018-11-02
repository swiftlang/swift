// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_autoclosure %s | %FileCheck %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_autoclosure.call_auto_closure()
// CHECK-NEXT: [[@LINE+3]]:26 -> [[@LINE+10]]:2 : 0
// CHECK-NEXT: }

func call_auto_closure() {
// CHECK-LABEL: sil_coverage_map {{.*}}// use_auto_closure #1 (@autoclosure () -> Swift.Bool) -> Swift.Bool in coverage_autoclosure.call_auto_closure()
// CHECK-NEXT: [[@LINE+1]]:63 -> [[@LINE+3]]:4 : 0
  func use_auto_closure(_ x: @autoclosure () -> Bool) -> Bool {
    return x() // CHECK-COV: {{ *}}[[@LINE]]|{{ *}}1
  }
  let _ = use_auto_closure(false || true)
}
