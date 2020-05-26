// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_arg_ternary %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

var s: String?

// CHECK: sil_coverage_map {{.*}} "$s20coverage_arg_ternary1f0B0ySSSg_tF"
// CHECK-NEXT:  [[@LINE+2]]:43 -> [[@LINE+2]]:45 : 0
// CHECK-NEXT: }
func f(arg: String? = s != nil ? s : nil) {}
