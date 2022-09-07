// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_closure_returns_never %s | %FileCheck %s

// CHECK-LABEL: closure #1 (Swift.Never) -> Swift.Never in coverage_closure_returns_never.closure_with_fatal_error(Swift.Array<Swift.Never>) -> ()
// CHECK: increment_profiler_counter 0
// CHECK-NEXT: [[LOAD:%.*]] = load {{.*}} : $*Never
// CHECK-NEXT: debug_value [[LOAD]] : $Never
// CHECK-NEXT: unreachable

func closure_with_fatal_error(_ arr: [Never]) {
// CHECK-LABEL: sil_coverage_map {{.*}}// closure #1 (Swift.Never) -> Swift.Never
// CHECK-NEXT: [[@LINE+1]]:11 -> [[@LINE+3]]:4
  arr.map {
    _ in fatalError()
  }
}
