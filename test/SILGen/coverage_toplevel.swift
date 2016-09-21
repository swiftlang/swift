// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_toplevel %s | %FileCheck %s

// CHECK: sil_coverage_map{{.*}}__tlcd_line:[[@LINE+2]]:1
// CHECK:  [[@LINE+1]]:1 -> [[@LINE+1]]:11
print("a")

// CHECK: sil_coverage_map{{.*}}// coverage_toplevel.f1
func f1() {}

var i : Int32 = 0

// CHECK: sil_coverage_map{{.*}}__tlcd_line:[[@LINE+2]]:1
// CHECK:  [[@LINE+1]]:7 -> [[@LINE+1]]:15 : (0 + 1)
while (i < 10) {
  i += 1
}
