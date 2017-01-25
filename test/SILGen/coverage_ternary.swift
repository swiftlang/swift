// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_ternary %s | %FileCheck %s

// rdar://problem/23256795 - Avoid crash if an if_expr has no parent
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_ternary.bar.__allocating_init
class bar {
  var m1 = 0 == 1
             ? "false" // CHECK: [[@LINE]]:16 -> [[@LINE]]:23 : 1
             : "true"; // CHECK: [[@LINE]]:16 -> [[@LINE]]:22 : 0
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_ternary.foo
func foo(_ x : Int32) -> Int32 {
  return x == 3
             ? 9000 // CHECK: [[@LINE]]:16 -> [[@LINE]]:20 : 1
             : 1234 // CHECK: [[@LINE]]:16 -> [[@LINE]]:20 : (0 - 1)
}

foo(1)
foo(2)
foo(3)

