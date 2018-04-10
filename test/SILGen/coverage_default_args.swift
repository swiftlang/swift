// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_default_args %s | %FileCheck %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_default_args.closureInArgs(f: (Swift.Int) -> Swift.Int) -> ()
// CHECK-NEXT: [[@LINE+5]]:59 -> [[@LINE+7]]:2 : 0
// CHECK-NOT: [[@LINE+4]]:59 -> [[@LINE+6]]:2 : 0

// CHECK-LABEL: sil_coverage_map {{.*}}// closure #1 (Swift.Int) -> Swift.Int in default argument 0 of coverage_default_args.closureInArgs(f: (Swift.Int) -> Swift.Int) -> ()
// CHECK-NEXT: [[@LINE+1]]:41 -> [[@LINE+1]]:57 : 0
func closureInArgs(f : ((Int) -> Int) = { i1 in i1 + 1 }) {
  f(3)
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_default_args.hasDefaultArgs
// CHECK: [[@LINE+1]]:35 -> {{[0-9]+}}:2 : 0
func hasDefaultArgs(i : Int = 10) {
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_default_args.autoClosureInArgs
// CHECK: [[@LINE+1]]:50 -> {{[0-9]+}}:2 : 0
func autoClosureInArgs(b : Bool = true && false) {
}
