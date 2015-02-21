// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_default_args %s | FileCheck %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_default_args.closureInArgs
// CHECK: [[@LINE+2]]:39 -> [[@LINE+2]]:55 : 1
// CHECK: [[@LINE+1]]:57 -> {{[0-9]+}}:2 : 0
func closureInArgs(f : (Int -> Int) = { i1 in i1 + 1 }) {
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
